; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.tests
  "Core implementation of the FOPPL language grammar."
  (:require [anglican.runtime       :as anglican]
            [foppl.core             :as core]
            [foppl.compiler         :as compiler]
            [foppl.utils            :as utils]
            [foppl.sugar            :as sugar]
            [foppl.graph            :as graph]
            [foppl.graph-examples   :as examples]))


; ========================================
; ============= Sugar Tests ==============
; ========================================

(def sugar1 '(let [a 10 b 20 a (+ a 30)] (- b 10) a))
(def desugar1 (sugar/desugar-expression sugar1))
(compiler/compile-expression {} true desugar1)

(def sugar2 '(foreach 5
                     [x (range 1 6)
                      y [-1 -2 -3 -4 -5]]
                     (+ x y)
                     (- x y)))

(def desugar2 (sugar/desugar-expression sugar2))
(compiler/compile-expression {} true desugar2)


(def sugar3 '(loop 5 10 + 1 10 20 50))

(def desugar3 (sugar/desugar-expression sugar3))
(compiler/compile-expression {} true desugar3)

; =======================================
; ============= Core Tests ==============
; =======================================

; Define Procedures

(core/defn addr [a b]
        (+ a b))

(core/defn nested-let
        [a]
        (let [a 10]
          a))


; Let Tests

(def core1 '(let [a 10] (+ c (addr a 5))))
(compiler/compile-expression core/registered-functions true core1)

; If Tests

(def core2 '(if (> a b)
              (+ (- 10 5) (addr a 5))
              (* 2 b)))

(compiler/compile-expression core/registered-functions true core2)

; Sample Tests

(def core3 '(sample (core/normal 5 (sample (core/uniform-continuous 1 10)))))
(compiler/compile-expression core/registered-functions true core3)



(def core4 '(let [a (sample (core/normal 5 (sample (core/uniform-continuous 1 10))))]
              a))
(compiler/compile-expression core/registered-functions true core4)


(def core5 '(let [a (sample (core/normal 5 5))]
              (sample (core/uniform-continuous (+ (- 10 5))
                                             (addr a 5)
                                          50))))
(compiler/compile-expression core/registered-functions true core5)

; Observe Tests

(def core6 '(let [a (sample (core/normal 5 5))]
              (observe (core/uniform-continuous (+ (- 10 5))
                                             (addr a 5)
                                          50)
                       40)))
(compiler/compile-expression core/registered-functions true core6)



(def core7 '(let [a (sample (core/normal 5 5))]
              (if (> a 2)
                (observe (core/uniform-continuous 10 20) 15)
                (observe (core/uniform-continuous 0 10) 5))))
(compiler/compile-expression core/registered-functions true core7)

; check that data-structures are compiled properly.

(def core8 '[ (+ 5 10) (- 10 4) (sample core/normal 5 5)])
(compiler/compile-expression core/registered-functions true core8)


(def core9 '{ :a (+ 5 10) :b (- 10 4) :c (sample core/normal 5 5)})
(compiler/compile-expression core/registered-functions true core9)

(def core10 '(:a (+ 5 10) :b (- 10 4) :c (sample core/normal 5 5)))
(compiler/compile-expression core/registered-functions true core10)

; check that nested let statements are handled correctly.

(def core11 '(let [a 10]
               (let [b 20]
                 (let [a b]
                   a))))
(compiler/compile-expression core/registered-functions true core11)

(def core12 '(nested-let 20))
(compiler/compile-expression core/registered-functions true core12)



; =======================================
; ============= Graph Tests =============
; =======================================

(let [[E G]             (compiler/compile-expression core/registered-functions true core3)
      num-vertices      (graph/count-vertices G)
      num-edges         (graph/count-edges G)
      prior-sample      (graph/sample-from-prior G)
      joint-sample      (graph/sample-from-joint G)]
  [num-vertices
   num-edges
   prior-sample
   joint-sample])
