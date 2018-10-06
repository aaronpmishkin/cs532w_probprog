; @Author: aaronmishkin
; @Date:   18-10-04
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-04

(ns auto-diff.test
  "Core implementation of the reverse-mode automatic differentiator."
  (:require [anglican.runtime       :as anglican]
            [auto-diff.core         :refer :all]
            [auto-diff.finite-diff  :as finite-diff]))


; BASIC TESTS

(def program1 '(fn [a]
                 (+ a 2)))

(def finite-diff1 (finite-diff/finite-difference-grad program1))
((eval finite-diff1) 10)
(str (auto-diff program1 [10]))

(def program2 '(fn [a b c]
                 (/ (+ a (* 7 b)) (sin c))))
(def finite-diff2 (finite-diff/finite-difference-grad program2))
((eval finite-diff2) 10 2 1)
(str (auto-diff program2 [10 2 1]))

(def program3 '(fn [a b c]
                 (if (> a 10)
                   (* b 10)
                   (+ (/ c 10) (/ c 10)))))


(def finite-diff3 (finite-diff/finite-difference-grad program3))
((eval finite-diff3) 11 3 20)
(str (auto-diff program3 [11 3 20]))

(def program4 '(fn [a b c]
                 (/ a b c)))

(def finite-diff4 (finite-diff/finite-difference-grad program4))
((eval finite-diff4) 10 2 5)
(str (auto-diff program4 [10 2 5]))


; TESTS WITH BRANCHES

(def program5 '(fn [mu sigma]
                 (/ (exp (- (/ (square (- 1
                                          mu))
                               (* 2 (square sigma)))))
                    (pow (* 2
                            Math/PI
                            (square sigma))
                         0.5))))

(def finite-diff5 (finite-diff/finite-difference-grad program5))
((eval finite-diff5) 2 3)
(str (auto-diff program5 [2 3]))
