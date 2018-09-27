; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.core
  "Core implementation of the FOPPL language grammar."
  (:require [anglican.runtime       :as anglican]
            [foppl.compiler         :as compiler]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def prob-program)
(def let 'let)
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
          [name e]
          (let [rho                 registered-functions
                [clean-rho clean-e] (compiler/desugar-program rho e)
                [E G]               (compiler/compile-expression clean-rho
                                                                 true
                                                                 clean-e)]
            E))
