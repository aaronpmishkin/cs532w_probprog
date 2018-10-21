; @Author: aaronmishkin
; @Date:   18-10-04
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-04

(ns auto-diff.differs
  "Differentiation rules for scalar-valued functions."
  (:require [anglican.runtime       :as anglican]
            [auto-diff.utils        :as utils]))


; ================================================
; ============ Pre-declare Functions =============
; ================================================


(def diff-expression)
(def diff-mult)
(def diff-div)
(def diff-add)
(def diff-sub)
(def diff-exp)
(def diff-log)
(def diff-sin)
(def diff-cos)
(def diff-tan)
(def square)
(def diff-pow)

; ================================================
; =============== Main Diff Block ================
; ================================================

; TODO: Add diff methods for more complex expressions (normal-pdf, etc.)

(defn diff-expression
  [e v V]
  (let [head            (first e)
        body            (rest e)]
    (cond
      (= '* head)       (diff-mult  body v V)
      (= '/ head)       (diff-div   body v V)
      (= '+ head)       (diff-add   body v V)
      (= '- head)       (diff-sub   body v V)
      (= 'exp head)     (diff-exp   body v V)
      (= 'log head)     (diff-log   body v V)
      (= 'sin head)     (diff-sin   body v V)
      (= 'cos head)     (diff-cos   body v V)
      (= 'tan head)     (diff-tan   body v V)
      (= 'pow head)     (diff-pow   body v V)
      (= 'square head)  (diff-mult  (list (first body)
                                          (first body))
                                    v
                                    V))))


; ===============================================
; ============= Individual Differs ==============
; ===============================================

(defn diff-mult
  [body v V]
  (cond
    (= (first body) (second body) v)    (* 2 (utils/lookup v V))
    (= (first body) v)                  (utils/lookup (second body)
                                                      V)
    (= (second body) v)                 (utils/lookup (first body)
                                                      V)
    :else                               0))

(defn diff-div
  [body v V]
  (let [first-val                       (utils/lookup (first body)
                                                      V)
        second-val                      (utils/lookup (second body)
                                                      V)]
    (cond
      (= (first body) (second body) v)    0
      (= (first body) v)                  (/ 1 second-val)
      (= (second body) v)                 (- (/ first-val
                                                (square second-val)))
      :else                               0)))

(defn diff-add
  [body v V]
  (cond
    (= (first body) (second body) v)    2
    (= (first body) v)                  1
    (= (second body) v)                 1
    :else                               0))

(defn diff-sub
  [body v V]
  (cond
    (and (= (count body) 1)
         (= (first body) v))            -1
    (= (first body) (second body) v)    0
    (= (first body) v)                  1
    (= (second body) v)                 -1
    :else                               0))

(defn diff-exp
  [body v V]
  (cond
    (= (first body) v)                  (anglican/exp (utils/lookup v V))
    :else                               0))

(defn diff-log
  [body v V]
  (cond
    (= (first body) v)                  (/ 1 (utils/lookup v V))
    :else                               0))

(defn diff-sin
  [body v V]
  (cond
    (= (first body) v)                  (anglican/cos (utils/lookup v V))
    :else                               0))

(defn diff-cos
  [body v V]
  (cond
    (= (first body) v)                  (- (anglican/sin (utils/lookup v V)))
    :else                               0))

(defn diff-tan
  [body v V]
  (cond
    (= (first body) v)                  (+ 1 (square (anglican/tan (utils/lookup v V))))
    :else                               0))

(defn diff-pow
  [body v V]
  (let [first-val                       (utils/lookup (first body)
                                                      V)
        second-val                      (utils/lookup (second body)
                                                      V)]
    (cond
      (= (first body) v)                (* second-val
                                          (Math/pow first-val
                                                    (dec second-val)))
      (= (second body) v)               (* (anglican/log first-val)
                                           (Math/pow first-val
                                                     second-val))
      :else                             0)))

; ==============================================
; =========== Convenience Functions ============
; ==============================================

(defn square
  [x]
  (* x x))
