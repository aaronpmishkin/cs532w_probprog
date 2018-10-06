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
((eval finite-diff1) 10)
(str (auto-diff program1 [10]))


; PROGRAM 2
(def program2
  '(fn [x y] (+ (* x x) (sin x))))
(def finite-diff2 (finite-diff/finite-difference-grad program2))
((eval finite-diff2) 3.5 7)
(str (auto-diff program2 [3.5 7]))


; PROGRAM 3
(def program3
  '(fn [x] (if (> x 5) (* x x) (+ x 18))))
(def finite-diff3 (finite-diff/finite-difference-grad program3))
((eval finite-diff3) -12.4)
(str (auto-diff program3 [-12.4]))

(def finite-diff3-2 (finite-diff/finite-difference-grad program3))
((eval finite-diff3-2) 15.1)
(str (auto-diff program3 [15.1]))


; PROGRAM 4
(def program4
  '(fn [x] (log x)))
(def finite-diff4 (finite-diff/finite-difference-grad program4))
((eval finite-diff4) 15.1)
(str (auto-diff program4 [15.1]))


; PROGRAM 5
(def program5
  '(fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu))
                               (* 2 (* sigma sigma))))
                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma))))))))
(def finite-diff5 (finite-diff/finite-difference-grad program5))
((eval finite-diff5) 2 1 5.5)
(str (auto-diff program5 [2 1 5.5]))


; PROGRAM 6
(def program6
  '(fn [x mu sigma] (normpdf x mu sigma)))
(def finite-diff6 (finite-diff/finite-difference-grad program6))
((eval finite-diff6) 2 1 5.5)
(str (auto-diff program6 [2 1 5.5]))

; Macroexpanded program

(def program6-2
  '(fn [x mu sigma]
     (/
       (exp (- (/ (square (- x mu)) (* 2 (square sigma)))))
       (pow (* 2 3.141592653589793 (square sigma)) 0.5))))

(def finite-diff6-2 (finite-diff/finite-difference-grad program6-2))
((eval finite-diff6-2) 2 1 5.5)
(str (auto-diff program6-2 [2 1 5.5]))


; PROGRAM 7
(def program7
  '(fn [x1 x2 x3] (+ (+ (normpdf x1 2 5)
                        (if (> x2 7)
                          (normpdf x2 0 1)
                          (normpdf x2 10 1)))
                    (normpdf x3 -4 10))))

(def finite-diff7 (finite-diff/finite-difference-grad program7))
((eval finite-diff7) 2 1 5.5)
(str (auto-diff program7 [2 1 5.5]))


(def finite-diff7-2 (finite-diff/finite-difference-grad program7))
((eval finite-diff7-2) 3 8 -3.21)
(str (auto-diff program7 [3 8 -3.21]))
