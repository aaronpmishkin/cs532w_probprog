(ns foppl.scoring
  "Core graph operations for the FOPPL language."
  (:require [anglican.runtime     :as anglican]
            [clojure.walk         :as walk]
            [foppl.distributions  :refer :all]
            [foppl.utils          :as utils]))



; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def score-vertex)
(def score-assignment)
(def parse-density-expression)
(def parse-if-for-density)
(def evaluate-if-density)
(def evaluate-density)

; ============================================
; ================= Scoring ==================
; ============================================

(defn score-vertex
  [value-map P v]
  (let [e       (get P v)
        e       (walk/prewalk-replace value-map
                                      e)
        E       (evaluate-density e)]
    E))

(defn score-assignment
  [value-map G sort?]
  (let [P       (get G :P)
        V       (if sort?
                  (utils/topological-sort G)
                  (get G :V))]
    (reduce (fn [acc v]
              (+ acc
                 (score-vertex value-map
                               P
                               v)))
            0
            V)))


; ============================================
; =========== Density Expressions ============
; ============================================

(defn parse-density-expression
  [e]
  (let [head (first e)
        body (rest e)]
       (cond
         (= 'if head)           (parse-if-for-density body)
         (= 'anglican.runtime/observe* head)     (first body))))

(defn parse-if-for-density
  [body]
  (let [[e1 e2 e3]      body
        E1              (eval e1)]
       (if E1
         (parse-density-expression e2)
         e3)))

(defn evaluate-if-density
  [E]
  (let [[_ E1 E2 E3]        E]
    (if (eval E1)
      (eval E2)
      0)))

(defn evaluate-density
  [E]
  (if (= 'if (first E))
    (evaluate-if-density E)
    (eval E)))
