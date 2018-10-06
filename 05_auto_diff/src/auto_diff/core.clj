; @Author: aaronmishkin
; @Date:   18-10-04
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-04

(ns auto-diff.core
  "Core implementation of the reverse-mode automatic differentiator."
  (:require [anglican.runtime       :as anglican]
            [clojure.walk           :as walk]
            [auto-diff.sugar        :as sugar]
            [auto-diff.forward      :as forward]
            [auto-diff.backward     :as backward]
            [auto-diff.utils        :as utils]))


; ================================================
; ============ Pre-declare Functions =============
; ================================================

(def auto-diff)


; ================================================
; =============== Extra Functions ================
; ================================================

(def sin anglican/sin)
(def cos anglican/cos)
(def tan anglican/tan)
(def exp anglican/exp)
(def log anglican/log)
(defn pow
  [base exponent]
  (Math/pow base exponent))
(defn square
  [x]
  (pow x 2))


(defmacro normpdf
          [x mu sigma]
          (list '/ (list 'exp (list '- (list '/ (list 'square (list '- x
                                                                       mu))
                                              (list '* 2 (list 'square sigma)))))
                (list 'pow (list '*
                                 2
                                 Math/PI
                                 (list 'square sigma))
                      0.5)))

; ================================================
; ============= Entry for Auto-Diff ==============
; ================================================

(defn auto-diff
  [f-expression param-values]
  (let [head                (first f-expression)
        params              (second f-expression)
        body                (last f-expression)
        body                (walk/postwalk sugar/desugar-expression
                                           body)
        G                   (utils/init-graph params
                                              param-values)
        [v E G]             (forward/forward-expression body G :root)
        G                   (utils/compute-fanout G)
        vf                  (utils/get-root-vertex G)
        G                   (backward/backward vf G)
        gradient            (utils/make-gradient-map params G)]
    [E gradient G]))
