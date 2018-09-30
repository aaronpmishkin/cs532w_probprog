; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.graph
  "Core graph operations for the FOPPL language."
  (:require [anglican.runtime     :as anglican]
            [clojure.walk         :as walk]
            [foppl.utils          :as utils]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def create-graph)
(def merge-graphs)
(def merge-nested)
(def merge-graph-list)
(def create-edges)
(def print-graph)
(def count-vertices)
(def count-children)
(def count-edges)
(def is-prior?)
(def sample-vertex)
(def sample-from-graph)
(def sample-from-prior)
(def sample-from-joint)

; =============================================
; ============== Graph Creation ===============
; =============================================

(defn create-graph
  []
  {:V []
   :A {}
   :P {}
   :Y {}})


(defn merge-graphs
  [G1 G2]
  (let [V (utils/merge-vectors (get G1 :V)
                         (get G2 :V))
        A (merge-nested (get G1 :A)
                        (get G2 :A))
        P (merge (get G1 :P)
                 (get G2 :P))
        Y (merge (get G1 :Y)
                 (get G2 :Y))]
    {:V V
     :A A
     :P P
     :Y Y}))

(defn merge-nested
  [map1 map2]
  (let [new-map (merge map1 map2)
        keys    (keys new-map)]
    (loop [map      new-map
           keys     keys]
      (if (empty? keys)
        map
        (recur (assoc map
                      (first keys)
                      (merge (get map1
                                  (first keys)
                                  {})
                             (get map2
                                  (first keys)
                                  {})))
               (rest keys))))))




(defn merge-graph-list
  [graphs]
  (reduce merge-graphs
          (first graphs)
          (rest graphs)))

(defn create-edges
  [A parents v]
  (reduce (fn [acc p]
            (assoc acc
                   p
                   (assoc (get acc p)
                          v
                          true)))
          A
          parents))

; =============================================
; ========== Basic Graph Operations ===========
; =============================================

(defn print-graph
  [graph]
  (if (string? graph)
    (let [val   (read-string graph)
          h     (println val)
          G     (if (vector? val)
                    (second val)
                    val)]
      (println G)
      G)
    (do
      (println graph)
      graph)))

(defn count-vertices
  [graph]
  (count (get graph :V)))

(defn count-children
  [child-map]
  (reduce (fn [acc dict-pair]
            (if (last dict-pair)
              (inc acc)
              acc))
          0
          (seq child-map)))

(defn count-edges
  [graph]
  (reduce (fn [acc vertex]
            (+ acc
               (count-children (last vertex))))
          0
          (seq (get graph :A))))


; =============================================
; ================= Sampling ==================
; =============================================

(defn is-prior?
  [graph v]
  (nil? (get (get graph
                  :Y)
             v)))

(defn sample-vertex
  [graph value-map P v]
  (let [e       (get P v)
        E       (walk/prewalk-replace value-map
                              e)
        dist    (utils/parse-density-expression E)]
    (if (= dist 1)
      nil
      (anglican/sample* (eval dist)))))


(defn sample-from-graph
  [graph prior-only]
  (let [P (get graph :P)
        V (utils/topological-sort graph)]
    (reduce (fn [sample-map v]
              (if (or (not prior-only)
                      (is-prior? graph v))
                (assoc sample-map
                       v
                       (sample-vertex graph
                                      sample-map
                                      P
                                      v))
                sample-map))
            {}
            V)))

(defn sample-from-prior
  [graph]
  (sample-from-graph graph
                     true))

(defn sample-from-joint
  [graph]
  (sample-from-graph graph
                     false))
