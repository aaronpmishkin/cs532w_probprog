; @Author: aaronmishkin
; @Date:   18-11-08
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-11-08

(ns tests
  "Tests for the HOPPL language."
  (:require [hoppl.core           :refer :all]))

; Constants:

; Test Constants:
(probabilistic-program
  10)

; Test Constant Vectors:
(probabilistic-program
  [1 2 3 4 5])

; Test Constant Maps:
(probabilistic-program
  {:a 10 :b 20 :c :d})

; Expressions
(probabilistic-program (+ 1 1))

(defn adder
  [v1 v2 v3]
  (+ v1 v2 v3))

(probabilistic-program
  (adder (adder 1 1 2) 5 6))

; If Statements:
(probabilistic-program
  (if (= (adder 1 2 1) 4)
    (nth [1 2 3 4] 3)
    (get {:a 10 :b 20} :a)))

; Anonymous Functions:
(probabilistic-program
  (let [a   (fn [v1 v2]
              (+ v1 v2))
        b   (a 10 5)
        c   ((fn [v1 v2]
               (- v1 v2))
             2 1)]
    (+ b c)))



;
