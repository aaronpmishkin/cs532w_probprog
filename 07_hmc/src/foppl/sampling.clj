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

(def sample-vertex)
(def sample-from-graph)
(def ordered-sample-from-graph)
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


(defn sample-vertex
  [graph value-map P v]
  (let [e       (get P v)
        E       (walk/prewalk-replace value-map
                              e)
        dist    (scoring/parse-density-expression E)
        sample  (if (= dist 1)
                  nil
                  (anglican/sample* (eval dist)))]
    (if (seq? sample)
      (into [] sample)
      sample)))



(defn sample-from-graph
  [graph prior-only sort?]
  (let [P (get graph :P)
        V (if sort
            (utils/topological-sort graph)
            (get graph :V))]
    (reduce (fn [sample-map v]
              (if (or (not prior-only)
                      (graph/is-prior? graph v))
                (assoc sample-map
                       v
                       (sample-vertex graph
                                      sample-map
                                      P
                                      v))
                sample-map))
            {}
            V)))

(defn ordered-sample-from-graph
  [graph prior-only]
  (let [P (get graph :P)
        V (get graph :V)]
    (reduce (fn [[acc sample-map] v]
              (if (or (not prior-only)
                      (graph/is-prior? graph v))
                (let [sample (sample-vertex graph
                                            sample-map
                                            P
                                            v)]
                  [(conj acc sample) (assoc sample-map v sample)])

                [acc sample-map]))
            [[] {}]
            V)))

(defn sample-from-prior
  [graph sort?]
  (sample-from-graph graph
                     true
                     sort?))


(defn sample-from-joint
  [graph sort?]
  (sample-from-graph graph
                     false
                     sort?))


; =============================================
; ============ Inference Samplers =============
; =============================================

(defn sample-from-consistent-joint
  [G sort?]
  (let [Y                   (get G :Y)
        ks                  (keys Y)
        value-map           (sample-from-prior G sort?)]
    (reduce (fn [acc, k]
              (assoc acc
                     k
                     (get Y k)))
            value-map
            ks)))

(defn vec-sample-from-consistent-joint
  [G index-map]
  (let [value-map           (sample-from-consistent-joint G false)
        value-vec           (utils/sample-to-vec index-map value-map)]
    [value-vec value-map]))


(defn sample-from-markov-blanket
  [v s G]
  (let [P                   (get G :P)
        v-sample            (sample-vertex G s P v)
        s-prime             (assoc s v v-sample)]
    s-prime))


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
