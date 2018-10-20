(ns foppl.inference
  "Core inference operations."
  (:require [clojure.walk           :as walk]
            [clojure.core.matrix    :as m]
            [anglican.runtime       :as anglican]
            [foppl.proposals        :as proposals]
            [foppl.mh-gibbs         :as mh-gibbs]
            [foppl.covariance       :as covariance]
            [foppl.sampling         :as sampling]
            [foppl.scoring          :as scoring]
            [foppl.utils            :as utils]))



; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def compute-expectation)
(def boolean?)
(def into-vec)
(def get-zeros)
(def sum-eval)
(def eval-E)
(def eval-at-samples)
(def mc-expectation)


; ===============================================
; =========== Inference Entry Point =============
; ===============================================

(defn compute-expectation
  [inf-method scale block-size E G]
  (let [n                   10
        burnin              10
        sorted-V            (utils/topological-sort G)
        G                   (assoc G :V sorted-V)
        [Sigma, index-map]  (covariance/compute-empirical-covariance G 100)
        Sigma               (m/mul Sigma scale)
        Q                   (proposals/retrieve-Q inf-method)
        [s-vec s-start]     (sampling/vec-sample-from-consistent-joint G index-map)
        p-tilde-start       (scoring/score-assignment s-start
                                                    G
                                                    false)
        s-list              (mh-gibbs/mh-in-gibbs Q G [s-start p-tilde-start] Sigma block-size index-map)
        s-list              (drop burnin s-list)
        samples             (take n s-list)
        acc                 (sum-eval E samples)
        mean                (mapv (fn [x] (/ x n)) (get acc :sum))
        variance            (mapv (fn [x y] (/ (- y (/ (* x x) n)) n)) (get acc :sum) (get acc :sum2))]
    {:mean mean :variance variance}))

; ===============================================
; =========== Expectation Utilities =============
; ===============================================

(defn boolean?
  [x]
  (instance? Boolean x))

(defn into-vec
  [x]
  (if (vector? x)
    (if (boolean? (first x))
      (walk/prewalk-replace {true 1 false 0} x)
      x)
    (if (boolean? x)
      [(walk/prewalk-replace {true 1 false 0} x)]
      [x])))

(defn get-zeros
  [n]
  (loop [n n
         acc []]
    (if (= 0 n)
      acc
      (recur (dec n)
             (conj acc 0)))))

(defn sum-eval
  [e s-list]
  (loop [acc        {:sum (get-zeros (count (into-vec (eval-E e (first s-list))))) :sum2 (get-zeros (count (into-vec (eval-E e (first s-list)))))}
         s-list     s-list]
    (if (empty? s-list)
      acc
      (let [E               (into-vec (eval-E e (first s-list)))
            sum             (mapv + (get acc :sum) E)
            sum2            (mapv (fn [x y] (+ x (* y y)))
                                  (get acc :sum2)
                                  E)
            acc             (assoc (assoc acc :sum sum) :sum2 sum2)]
        (recur acc
               (rest s-list))))))

(defn eval-E
  [e s]
  (let [E       (walk/prewalk-replace s e)
        val     (eval E)]
    val))

; (defn eval-at-samples
;   [e s-list]
;   (loop [s-list s-list
;          Es     []]
;     (if (empty? s-list)
;       Es
;       (recur (rest s-list)
;              (conj Es
;                    (eval-E e
;                            (first s-list)))))))


; (defn mc-expectation
;   [e s-list]
;   (let [Es      (basic-loop e s-list)]
;     (cond
;       (number? (first Es))      {:mean (anglican/mean Es) :variance (anglican/variance Es)}
;       (boolean? (first Es))     (let [Es (walk/prewalk-replace {true 1 false 0}
;                                                                Es)]
;                                   {:mean (anglican/mean Es) :variance (anglican/variance Es)})
;       :else                     {:mean (anglican/mean Es) :variance (anglican/variance Es)})))
