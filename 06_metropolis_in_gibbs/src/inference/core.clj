(ns inference.core
  "Core inference operations."
  (:require [clojure.walk           :as walk]
            [anglican.runtime       :as anglican]
            [inference.proposals    :as proposals]
            [inference.samplers     :as samplers]
            [foppl.utils            :as utils]
            [foppl.graph            :as graph]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def compute-expectation)
(def mc-expectation)
(def eval-E)
(def eval-at-samples)


; ===============================================
; =========== Inference Entry Point =============
; ===============================================

(defn compute-expectation
  [inf-method scale block-size E G]
  (let [n                   10
        burnin              10
        sorted-V            (utils/topological-sort G)
        G                   (assoc G :V sorted-V)
        [Sigma, index-map]  (graph/compute-empirical-covariance G 100)
        Q                   (proposals/retrieve-Q inf-method)
        s-list              (samplers/mh-in-gibbs Q G Sigma scale block-size index-map)
        s-list              (drop burnin s-list)
        samples             (take n s-list)
        mc-E                (mc-expectation E samples)]
    mc-E))

; ===============================================
; =========== Expectation Utilities =============
; ===============================================

(defn boolean? [x]
  (instance? Boolean x))

(defn mc-expectation
  [e s-list]
  (let [Es      (eval-at-samples e s-list)]
    (cond
      (number? (first Es))      {:mean (anglican/mean Es) :variance (anglican/variance Es)}
      (boolean? (first Es))     (let [Es (walk/prewalk-replace {true 1 false 0}
                                                               Es)]
                                  {:mean (anglican/mean Es) :variance (anglican/variance Es)})
      :else                     {:mean (anglican/mean Es) :variance (anglican/variance Es)})))

(defn eval-E
  [e s]
  (let [E       (walk/prewalk-replace s e)
        val     (eval E)]
    val))

(defn eval-at-samples
  [e s-list]
  (loop [s-list s-list
         Es     []]
    (if (empty? s-list)
      Es
      (recur (rest s-list)
             (conj Es
                   (eval-E e
                           (first s-list)))))))
