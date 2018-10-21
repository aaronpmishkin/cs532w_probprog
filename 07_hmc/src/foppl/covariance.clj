(ns foppl.covariance
  "Core graph operations for the FOPPL language."
  (:require [anglican.runtime     :as anglican]
            [anglican.runtime     :refer [observe*]]
            [clojure.core.matrix  :as m]
            [foppl.graph          :as graph]
            [foppl.sampling       :as sampling]
            [foppl.utils          :as utils]
            [foppl.distributions  :refer :all]))


(def pull-n-samples)
(def samples-to-matrix)
(def construct-index-map)
(def remove-discrete-vars)
(def compute-empirical-covariance)
(def conditional-covariance)
(def marginalize-covariance)

; ==============================================
; =========== Empirical Covariance =============
; ==============================================

(defn pull-n-samples
  [G n]
  (loop [n       n
         samples []]
    (if (= n 0)
      samples
      (recur (dec n)
             (conj samples
                   (sampling/sample-from-joint G false))))))

(defn samples-to-matrix
  [s-list]
  (m/matrix (map utils/c-sample-to-vec s-list)))

(defn construct-index-map
  [labels]
  (loop [acc    {}
         labels labels
         index  0]
    (if (empty? labels)
      acc
      (recur (assoc acc (first labels) index)
             (rest labels)
             (inc index)))))

(defn remove-discrete-vars
  [s-matrix s labels index-map G]
  (let [continuous-indices  (reduce (fn [acc v]
                                      (if (graph/is-continuous? v s G)
                                        (conj acc (get index-map v))
                                        acc))
                                    []
                                    labels)
        [index-map i]       (reduce (fn [[acc i] v]
                                        (if (graph/is-continuous? v s G)
                                          [(assoc acc v i) (inc i)]
                                          [acc i]))
                                    [{} 0]
                                    labels)
        columns              (map (fn [index] (m/get-column s-matrix index))
                                  continuous-indices)
        s-matrix             (m/transpose (m/matrix columns))]
    [s-matrix index-map]))


(defn compute-empirical-covariance
  [sampling-map G n]
  (let [V               (get G :V)
        samples         (repeatedly n (fn [] (sampling/sample-from-graph G sampling-map false)))
        value-map       (second (first samples))
        samples         (map second samples)
        samples         (map (fn [x] (map (fn [y]
                                            (seq? y) 0 y)
                                          x))
                             samples)
        labels          V
        index-map       (construct-index-map labels)
        s-matrix        (m/matrix samples)
        [s-matrix
         index-map]     (remove-discrete-vars s-matrix value-map labels index-map G)
        n               (count (keys index-map))
        observed-vars   (map first (into [] (get G :Y)))
        k               (count (reduce (fn [acc v] (if (get index-map v)
                                                     (conj acc v)
                                                     acc))
                                       []
                                       observed-vars))
        index-map       (reduce (fn [acc v] (dissoc acc v))
                                index-map
                                observed-vars)
        cov             (if (> n 0)
                          (conditional-covariance (anglican/covariance s-matrix)
                                                  n
                                                  k)
                          1)]
    [cov index-map]))


(defn conditional-covariance
  [M n k]
  (let [Sigma11         (m/submatrix M 0 (- n k) 0 (- n k))
        Sigma22         (m/submatrix M (- n k) k (- n k) k)
        Sigma12         (m/submatrix M 0 (- n k) (- n k) k)
        Sigma22-inv     (m/inverse Sigma22)
        Sigma           (m/sub Sigma11
                               (m/mmul Sigma12 (m/mmul Sigma22-inv
                                                       (m/transpose Sigma12))))]
    Sigma))


(defn marginalize-covariance
  [Sigma indices]
  (if (= 1 (count indices))
     (let [z (m/mget Sigma (first indices)
                           (first indices))]
       z)
     nil))
