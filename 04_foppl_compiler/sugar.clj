; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.sugar
  "Meta-functions for transforming FOPPL's syntactic sugar into the basic language grammar"
  (:require [foppl.utils       :as utils]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================


(def desugar-expression)
(def desugar-let)
(def desugar-let-helper)
(def desugar-foreach)
(def desugar-foreach-helper)
(def desugar-loop)
(def desugar-loop-helper)

; ==================================================
; ============= De-sugaring Functions ==============
; ==================================================

(defn desugar-expression
  [e]
  (cond (list? e)       (let [head           (first e)
                              body           (rest e)]
                          (cond
                            (= 'let head)     (desugar-let e)
                            (= 'foreach head) (desugar-foreach e)
                            (= 'loop head)    (desugar-loop e)
                            :else             e))
    (= '_ e)            (gensym "newvar_")
    :else               e))

; LET

(defn desugar-let
  [e]
  (let [[s assignments & expressions] e]
    (desugar-let-helper assignments expressions)))

(defn desugar-let-helper
  [assignments expressions]
  (do
    (if (empty? assignments)
      (if (empty? (rest expressions))
        (first expressions)
        (list 'let
              [(gensym "garbage_") (first expressions)]
              (desugar-let-helper assignments
                                  (rest expressions))))
      (list 'let
            [(first assignments) (second assignments)]
            (desugar-let-helper (rest (rest assignments))
                                expressions)))))


; FOREACH

(defn desugar-foreach
  [e]
  (let [[s c assignments & expressions] e
        assignment-pairs               (utils/pair-assignments assignments)]
    (loop [acc []
           index 0]
      (if (>= index c)
        (conj (apply list acc)
              'vector)
        (recur (conj acc
                     (desugar-foreach-helper index
                                             assignment-pairs
                                             expressions))
               (inc index))))))

(defn desugar-foreach-helper
  [index assignments expressions]
  (let [new-let-expression (apply list
                                  (reduce conj
                                          expressions
                                          (vector
                                                (reduce (fn [acc [v e]]
                                                          (conj (conj acc v)
                                                                (list 'nth
                                                                      e
                                                                      index)))
                                                        []
                                                        assignments)
                                                'let)))]
     (desugar-let new-let-expression)))

; LOOP

(defn desugar-loop
  [e]
  (let [[s c start-e f & expressions]   e
        h                               (println expressions)
        new-vars                        (utils/make-var-list expressions)
        assignment-block                (apply vector (interleave new-vars expressions))
        new-let-expression              (list 'let
                                              assignment-block
                                              (desugar-loop-helper 0
                                                                   c
                                                                   start-e
                                                                   f
                                                                   new-vars))]
    (desugar-let new-let-expression)))

(defn desugar-loop-helper
  [index c v f expressions]
  (if (>= index c)
    v
    (let [v-next (gensym "newvar_")
          start  [ f index v]
          f-call (utils/unroll-args start expressions)]
      (list 'let
            [v-next f-call]
            (desugar-loop-helper (inc index)
                                 c
                                 v-next
                                 f
                                 expressions)))))
