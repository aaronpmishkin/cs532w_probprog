; @Author: aaronmishkin
; @Date:   18-10-05
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-05

(ns auto-diff.sugar
  "Desugaring functions for automatic differentiation."
  (:require [anglican.runtime       :as anglican]
            [clojure.walk           :as walk]
            [auto-diff.utils        :as utils]))


; ================================================
; ============ Pre-declare Functions =============
; ================================================

(def desguar-expression)
(def desugar-variadic)

; ================================================
; ============ De-sugaring Functions =============
; ================================================

(defn desugar-expression
  [e]
  (cond (and (list? e)
             (> (count e)
                3))
        (let [head              (first e)
              body              (rest e)]
          (cond
            (= 'normpdf head)   (let [x (macroexpand-1 e)
                                      x (walk/postwalk desugar-expression x)]
                                     x)
            (= '+ head)         (desugar-variadic head body)
            (= '* head)         (desugar-variadic head body)
            (= '- head)         (list head
                                      (first body)
                                      (desugar-variadic '+ (rest body)))
            (= '/ head)         (list head
                                      (first body)
                                      (desugar-variadic '* (rest body)))
            :else           e))
    :else               e))




(defn desugar-variadic
  [head body]
  (if (> (count body)
         2)
    (list head
          (first body)
          (desugar-variadic head
                            (rest body)))
    (list head
          (first body)
          (second body))))
