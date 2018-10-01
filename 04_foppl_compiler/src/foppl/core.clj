; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.core
  "Core implementation of the FOPPL language grammar."
  (:require [anglican.runtime       :as anglican]
            [foppl.compiler         :as compiler]
            [clojure.core.matrix    :as m]
            [clojure.core           :as clj]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def prob-program)
(def if 'if)
(def sample 'sample)
(def observe 'observe)

; ===============================================
; ================ Distributions ================
; ===============================================

(def binomial anglican/binomial)
(def beta anglican/beta)
(def normal anglican/normal)
(def binomial anglican/binomial)
(def uniform-continuous anglican/uniform-continuous)
(def discrete anglican/discrete)

; ===============================================
; ============== Matrix Operations ==============
; ===============================================

(clj/defn mat-mul [& args] (apply m/mmul args))
(clj/defn mat-add [& args] (apply m/add args))
(clj/defn mat-transpose [& args] (apply m/transpose args))
(clj/defn mat-tanh [M] (m/emap anglican/tanh M))
(clj/defn mat-relu [M] (m/emap (fn [x] (if (> x 0) x 0)) M))
(clj/defn mat-repmat [M r c]
  (let [R (reduce (partial m/join-along 0) (repeat r M))]
    (reduce (partial m/join-along 1) (repeat c R))))


; ===============================================
; ============ Additional Operations ============
; ===============================================

(def tanh anglican/tanh)
(def sqrt anglican/sqrt)

; ===============================================
; ======== namespacing for the language =========
; ===============================================

(def registered-functions {})

; ===============================================
; ============= COMPILER Functions ==============
; ===============================================


(defmacro defn
          [fn-name params body]
          (def registered-functions
            (assoc registered-functions
                   fn-name
                   {:params params
                    :body   body}))
          nil)


(defmacro probabilistic-program
          [e]
          (let [rho                 registered-functions
                [clean-rho clean-e] (compiler/desugar-program rho e)
                [E G]               (compiler/compile-expression clean-rho
                                                                 true
                                                                 clean-e)]
            (str [E G])))


; ===============================================
; ============= FOPPL Functions ==============
; ===============================================

(defn append
  [vec item]
  (conj vec item))
