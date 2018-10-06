; @Author: aaronmishkin
; @Date:   18-10-04
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-04


(ns auto-diff.utils
  "Utilities for all aspects of the differentiator."
  (:require [clojure.walk           :as walk]
            [clojure.core.matrix    :as m]))


; ================================================
; ============ Pre-declare Functions =============
; ================================================


(def multiple-insert)
(def multiple-get)
(def separate-return-values)
(def init-graph)
(def compute-fanout)
(def count-parents)
(def get-children)
(def create-edges)
(def merge-vectors)
(def merge-graphs)
(def merge-nested)
(def merge-graph-list)
(def get-root-vertex)
(def get-details)
(def set-details)
(def lookup)
(def make-gradient-map)




; ================================================
; ============== General Utilities ===============
; ================================================

(defn multiple-insert
  [map key-value-pairs]
  (reduce (fn [map [key value]]
            (assoc map key value))
          map
          key-value-pairs))

(defn multiple-get
  [m keys]
  (mapv (fn [k]
            (get m k))
       keys))

(defn separate-return-values
  [eval-params]
  (let [Vs                  (map first
                                 eval-params)
        Es                  (map second
                                 eval-params)
        Gs                  (map (fn [vec]
                                     (nth vec 2))
                                 eval-params)]
    [Vs Es Gs]))

; ================================================
; =============== Graph Utilities ================
; ================================================

(defn init-graph
  [params param-values]
  (let [Vs                  (map (fn [param]
                                   (gensym "vertex_"))
                                 params)
        S                   (apply hash-map
                                   (interleave params
                                               Vs))
        details             (map (fn [param-value]
                                   {:value param-value
                                    :adjoint 0
                                    :type :variable})
                                 param-values)
        V                   (apply hash-map
                                   (interleave Vs
                                               details))
        G                   (multiple-insert {}
                                             [[:V V]
                                              [:S S]
                                              [:A {}]])]
    G))


(defn compute-fanout
  [G]
  (loop [ks         (keys (get G :V))
         G         G]
    (if (empty? ks)
      G
      (let [A           (get G :A)
            V           (get G :V)
            k           (first ks)
            details     (get V k)
            edges       (get A k)
            fanout      (count-parents edges)
            details     (assoc details
                               :fanout
                               fanout)
            G           (assoc G
                               :V
                               (assoc V
                                      k
                                      details))]
        (recur (rest ks)
               G)))))

(defn count-parents
  [edges]
  (reduce (fn [acc pair]
            (if (= (second pair)
                   :parent)
              (inc acc)
              acc))
          0
          (into [] edges)))

(defn get-children
  [v A]
  (reduce (fn [acc pair]
            (if (= (second pair)
                   :child)
              (conj acc
                    (first pair))
              acc))
          []
          (into [] (get A v))))

(defn create-edges
  [A p Vs]
  (reduce (fn [acc vi]
            (let [p-map     (get acc p)
                  p-map     (assoc p-map
                                   vi
                                   :child)
                  vi-map    (get acc vi)
                  vi-map    (assoc vi-map
                                   p
                                   :parent)
                  acc       (multiple-insert acc
                                            [[p p-map]
                                             [vi vi-map]])]
              acc))
          A
          Vs))


(defn merge-vectors
  [v1 v2]
  (reduce conj v1 v2))

(defn merge-graphs
  [G1 G2]
  (let [V (merge-vectors (get G1 :V)
                         (get G2 :V))
        A (merge-nested (get G1 :A)
                        (get G2 :A))
        S (merge (get G1 :S)
                 (get G2 :S))]
    {:V V
     :A A
     :S S}))

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

(defn get-root-vertex
  [G]
  (reduce (fn [acc pair]
            (let [v         (first pair)
                  details   (second pair)]
              (if (= (get details :type)
                     :root)
                v
                acc)))
          nil
          (into [] (get G :V))))




; ===============================================
; ============== Vertex Utilities ===============
; ===============================================

(defn get-details
  [v G]
  (get (get G :V) v))

(defn set-details
  [v details G]
  (assoc G
         :V
         (assoc (get G
                     :V)
                v
                details)))

(defn lookup
  [v V]
  (let [details (get V v)
        value   (get details :value)]
    value))



; ===============================================
; ============ Gradient Constructor =============
; ===============================================


(defn make-gradient-map
  [params G]
  (reduce (fn [acc p]
            (let [S             (get G :S)
                  v             (get S p)
                  details       (get-details v G)
                  df            (get details :adjoint)]
              (assoc acc
                     p
                     df)))
          {}
          params))
