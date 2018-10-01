; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.graph-examples
  "Core structures of the FOPPL language grammar."
  (:require [anglican.runtime :as anglican]))

; Define several examples of the graph data structure

; A small graph with 2 vertices and 1 edge.
(def small-graph
   ; vector of nodes in the graphical model.
  {:V [:y :mu]
   ; data structure for edges; this is a **directed** adjacency matrix.
   :A {:mu  {:y true}
       :y   {}}
   ; deterministic expression for probability mass functions.
   :P {:mu  (fn [value-map]
              (anglican/normal 0
                               1))
       :y   (fn [value-map]
              (anglican/normal (get value-map :mu)
                               1))}
   ; observations and their on-control-flow predicates
   :Y {:y [1 true]}})

; A larger graph with 5 vertices and 4 edges.
(def large-graph
   ; vector of nodes in the graphical model.
  {:V [:y :z :sigma :mu :rho]
   ; data structure for edges; this is a **directed** adjacency matrix.
   :A {:mu  {:y true
             :z true}
       :sigma {:y true}
       :rho {:z true}
       :y   {}}
   ; deterministic expression for probability mass functions.
   :P {:mu  (fn [value-map]
              (anglican/normal 0
                               1))
       :sigma  (fn [value-map]
                  (anglican/normal 10
                                   1))
       :rho  (fn [value-map]
               (anglican/normal 10
                                1))
       :y   (fn [value-map]
              (anglican/normal (get value-map :mu)
                               (get value-map :sigma)))
       :z   (fn [value-map]
              (anglican/normal (get value-map :mu)
                               (get value-map :rho)))}
   ; observations and their on-control-flow predicates
   :Y {:y [1 true] :z [1 true]}})
