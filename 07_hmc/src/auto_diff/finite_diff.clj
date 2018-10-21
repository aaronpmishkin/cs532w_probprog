; @Author: aaronmishkin
; @Date:   18-10-05
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-05

(ns auto-diff.finite-diff
  "Implementation of differentiation by finite differences."
  (:require [auto-diff.utils        :as utils]))


; ================================================
; ============ Pre-declare Functions =============
; ================================================


(def addd)
(def finite-difference-expr)
(def finite-difference-grad)


; ================================================
; ================== Main Code ===================
; ================================================


(defn addd [exprl i d]
  (if (= i 0)
    (reduce conj [`(~'+ ~d ~(first exprl))] (subvec exprl 1))
    (reduce conj (subvec exprl 0 i)
            (reduce conj [`(~'+ ~d ~(get exprl i))] (subvec exprl (+ i 1))))))

(defn finite-difference-expr [expr args i d]
  `(~'/ (~'- (~expr ~@(addd args i d)) (~expr ~@args)) ~d))

(defn finite-difference-grad [expr]
  (let [[op args body] expr
        d (gensym)
        fdes (map #(finite-difference-expr expr args % d) (range (count args)))
        argsyms (map (fn [x] `(~'quote ~x)) args)]
    `(~'fn [~@args]
       (~'let [~d 0.001]
         ~(zipmap argsyms fdes)))))
