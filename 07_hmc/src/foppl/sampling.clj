; @Author: aaronmishkin
; @Date:   18-10-13
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-20


(ns foppl.sampling
  "Core graph operations for the FOPPL language."
  (:require [anglican.runtime     :as anglican]
            [clojure.walk         :as walk]
            [foppl.distributions  :refer :all]
            [foppl.graph          :as graph]
            [foppl.scoring        :as scoring]
            [foppl.utils          :as utils]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def get-nils)
(def replace-first-nil)
(def sample-vertex)
(def sample-from-graph)
(def sample-from-prior)
(def sample-from-joint)
(def sample-from-consistent-joint)
(def vec-sample-from-consistent-joint)
(def sample-from-markov-blanket)
(def sample-prior-vertex-label)
(def sample-n-prior-vertex-labels)


; ==============================================
; ============== Graph Samplers ================
; ==============================================

(defn joint-sample-to-vec
  [V sample]
  (reduce (fn [acc v]
            (conj acc (get sample v)))
          []
          V))

(defn get-nils
  [n]
  (loop [n n
         acc []]
    (if (= 0 n)
      acc
      (recur (dec n)
             (conj acc nil)))))

(defn replace-first-nil
  [s-vec s]
  (loop [acc   []
         s-vec s-vec
         s     s]
    (cond
      (empty? s-vec)            acc
      (= (first s-vec) nil)     (recur (conj acc s)
                                       (rest s-vec)
                                       nil)
      :else                     (recur (conj acc (first s-vec))
                                       (rest s-vec)
                                       s))))

(defn sample-vertex
  [sampling-fn s-vec]
  (let [sample      (sampling-fn s-vec)
        sample      (if (seq? sample)
                      (into [] sample)
                      sample)]
    sample))


(defn sample-from-graph
  [G sampling-map prior-only]
  (let [V           (get G :V)
        s-vec       (get-nils (count V))]
    (reduce (fn [[sample s-vec] v]
              (if (or (not prior-only)
                      (graph/is-prior? G v))
                (let [v-s    (sample-vertex (get sampling-map v) s-vec)]
                  [(assoc sample v v-s)
                   (replace-first-nil s-vec v-s)])
                [sample s-vec]))
            [{} s-vec]
            V)))

(defn sample-from-prior
  [G sampling-map]
  (first (sample-from-graph G
                            sampling-map
                            true)))

(defn sample-from-joint
  [G sampling-map]
  (first (sample-from-graph G
                            sampling-map
                            false)))


; =============================================
; ============ Inference Samplers =============
; =============================================

(defn sample-from-consistent-joint
  [G sampling-map]
  (let [Y                   (get G :Y)
        ks                  (keys Y)
        [s s-vec]           (sample-from-graph G sampling-map true)
        s                   (reduce (fn [acc, k]
                                      (assoc acc
                                             k
                                             (get Y k)))
                                    s
                                    ks)
        s-vec               (joint-sample-to-vec (get G :V) s)]
    [s s-vec]))

(defn sample-from-markov-blanket
  [v sample s-vec G sampling-map]
  (let [v-sample            (sample-vertex (get sampling-map v) s-vec)
        s-prime             (assoc sample v v-sample)
        s-prime-vec         (joint-sample-to-vec (get G :V) s-prime)]
    [s-prime s-prime-vec]))


; ==============================================
; ============== Vertex Samplers ================
; ==============================================

(defn sample-prior-vertex-label
  [G]
  (let [V                   (get G :V)
        prior-V             (graph/get-prior-vertices G V)
        num-vertices        (count prior-V)
        index               (anglican/sample* (anglican/uniform-discrete 0 num-vertices))
        v                   (nth prior-V index)]
    v))

(defn sample-n-prior-vertex-labels
  [n G]
  (loop [n      n
         labels []]
    (if (= n 0)
      labels
      (let [l           (sample-prior-vertex-label G)
            chosen?     (reduce (fn [acc v] (or acc (= v l)))
                                false
                                labels)]
        (if chosen?
          (recur n labels)
          (recur (dec n)
                 (conj labels l)))))))
