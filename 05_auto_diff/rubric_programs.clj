; @Author: aaronmishkin
; @Date:   18-10-05
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-05

(ns auto-diff.rubric-programs
  "Rubic programs for reverse mode automatic differentiator."
  (:require [anglican.runtime       :as anglican]
            [auto-diff.core         :refer :all]
            [auto-diff.finite-diff  :as finite-diff]))


; PROGRAM 1
(def program1
  '(fn [x] (exp (sin x))))
(def finite-diff1 (finite-diff/finite-difference-grad program1))
((eval finite-diff1) 0)
(str (auto-diff program1 [0]))


; PROGRAM 2
(def program2
  '(fn [x y] (+ (* x x) (sin x))))
(def finite-diff2 (finite-diff/finite-difference-grad program2))
((eval finite-diff2) 0 10)
(str (auto-diff program2 [0 10]))


; PROGRAM 3
(def program3
  '(fn [x] (if (> x 5) (* x x) (+ x 18))))
(def finite-diff3 (finite-diff/finite-difference-grad program3))
((eval finite-diff3) 5.000001)
(str (auto-diff program3 [5.000001]))



; PROGRAM 4
(def program4
  '(fn [x] (log x)))
(def finite-diff4 (finite-diff/finite-difference-grad program4))
((eval finite-diff4) 0.1)
(str (auto-diff program4 [0.1]))


; PROGRAM 5
(def program5
  '(fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu))
                               (* 2 (* sigma sigma))))
                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma))))))))
(def finite-diff5 (finite-diff/finite-difference-grad program5))
((eval finite-diff5) 10 0 2)
(str (auto-diff program5 [10 0 2]))


; PROGRAM 6
(def program6
  '(fn [x mu sigma] (normpdf x mu sigma)))
(def finite-diff6 (finite-diff/finite-difference-grad program6))
((eval finite-diff6) 10 0 2)
(str (auto-diff program6 [10 0 2]))

; PROGRAM 7
(def program7
  '(fn [x1 x2 x3] (+ (+ (normpdf x1 2 5)
                        (if (> x2 7)
                          (normpdf x2 0 1)
                          (normpdf x2 10 1)))
                    (normpdf x3 -4 10))))

(def finite-diff7 (finite-diff/finite-difference-grad program7))
((eval finite-diff7) 2 7.01 5)
(str (auto-diff program7 [2 7.01 5]))
