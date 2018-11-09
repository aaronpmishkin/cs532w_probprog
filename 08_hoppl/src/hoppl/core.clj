; @Author: aaronmishkin
; @Date:   18-11-08
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-11-08

(ns hoppl.core
  "Core implementation of the HOPPL."
  (:require [anglican.runtime       :as anglican]
            [clojure.core           :as clj]
            [clojure.core.matrix    :as m]
            [hoppl.interpreter      :as interp]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def prob-program)
(def if 'if)
(def sample 'sample)
(def observe 'observe)
(def get-sample-list)
(def compute-expectation)

; ===============================================
; ============ Additional Operations ============
; ===============================================

(def tanh anglican/tanh)
(def sqrt anglican/sqrt)
(def log anglican/log)
(def exp anglican/exp)

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
          [n-samples e]
          (let [rho                     registered-functions
                [clean-rho clean-e]     (interp/desugar-program rho e)
                sigma                   {}
                sample-gen              (get-sample-list clean-rho
                                                         sigma
                                                         clean-e)
                samples                 (take n-samples sample-gen)
                E                       (compute-expectation samples)]
            E))


; ===============================================
; =========== Expectation Computation ===========
; ===============================================

(clj/defn get-sample-list
          [rho sigma e]
          (lazy-seq (cons (interp/evaluate-expression rho
                                                      sigma
                                                      0
                                                      e)
                          (get-sample-list rho sigma e))))


(clj/defn compute-expectation
  [samples]
  (loop [weighted-sum     0
         weight-total     0
         samples          samples]
    (if (empty? samples)
      (/ weighted-sum weight-total)
      (recur (+ weighted-sum
                (* (first (first samples))
                   (exp (second (first samples)))))
             (+ weight-total
                (exp (second (first samples))))
             (rest samples)))))



; ==============================================
; ============== HOPPL Functions ===============
; ==============================================

(defn append
  [vec item]
  (conj vec item))
