; @Author: aaronmishkin
; @Date:   18-11-08
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-11-08

(ns hoppl.sugar
  "Meta-functions for transforming HOPPL's syntactic sugar into the basic language grammar"
  (:require [hoppl.utils       :as utils]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================


(def desugar-expression)
(def desugar-let)
(def desugar-let-helper)
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
                            (= 'loop head)    (desugar-loop e)
                            :else             e))
    (= '_ e)            (gensym "newvar_")
    :else               e))

(defn desugar-let
  [e]
  (let [[s assignments & expressions] e]
    (desugar-let-helper assignments expressions)))



(defn desugar-let-helper
  [assignments expressions]
  (if (empty? assignments)
    (if (empty? (rest expressions))
      (first expressions)
      `((~'fn [~(gensym "garbage_")]
              ~(desugar-let-helper assignments (rest expressions)))
        ~(first expressions)))
    `((~'fn [~(first assignments)]
            ~(desugar-let-helper (rest (rest assignments))
                                 expressions))
      ~(second assignments))))


(defn desugar-loop
  [e]
  (let [[s c start-e f & expressions]   e
        Vars                            (repeatedly (count expressions)
                                                    (fn [] (gensym "var_")))
        assignments                     (interleave Vars
                                                    expressions)
        loop-expression                 `(~'let [~'bound                    ~c
                                                 ~'initial-value            ~start-e
                                                 ~@assignments
                                                 ~'g                (~'fn [~'i ~'w]
                                                                          (~f ~'i ~'w ~@Vars))]
                                                (~desugar-loop-helper 0 ~'bound ~'initial-value ~'g))]
    (desugar-let loop-expression)))




(defn desugar-loop-helper
  [i c v g]
  (if (= i c)
    v
    (let [v_next (g i v)]
      (desugar-loop-helper (inc i)
                   c
                   v_next
                   g))))



;
