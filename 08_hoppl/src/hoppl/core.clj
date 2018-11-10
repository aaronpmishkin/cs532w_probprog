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
(def normalize)
(def initialize-weighted-sum)
(def update-weighted-sum)
(def compute-bins)
(def normalize-bins)
(def initialize-bins)
(def update-bins)

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
          [n-samples return-type bins e]
          (let [rho                     registered-functions
                [clean-rho clean-e]     (interp/desugar-program rho e)
                sigma                   {}
                sample-gen              (get-sample-list clean-rho
                                                         sigma
                                                         clean-e)
                samples                 (take n-samples sample-gen)]
            (if (= return-type :mean)
              (compute-expectation samples)
              (compute-bins        samples bins))))


; ===============================================
; =========== Expectation Computation ===========
; ===============================================

(clj/defn get-sample-list
          [rho sigma e]
          (lazy-seq (cons (trampoline interp/evaluate-expression
                                      rho
                                      sigma
                                      0
                                      e)
                          (get-sample-list rho sigma e))))


(clj/defn compute-expectation
          [samples]
          (loop [acc              (initialize-weighted-sum (first samples))
                 samples          (rest samples)]
            (if (empty? samples)
              (normalize acc)
              (recur (update-weighted-sum acc
                                          (first samples))
                     (rest samples)))))

(clj/defn normalize
          [[weighted-sum weight-total]]
          (if (vector? weighted-sum)
            (mapv (fn [v] (/ v weight-total))
                 weighted-sum)
            (/ weighted-sum
               weight-total)))

(clj/defn initialize-weighted-sum
          [sample]
          (let [weight-total (exp (second sample))]
            (if (vector? (first sample))
              [(mapv (fn [v]
                        (* v
                           weight-total))
                    (first sample))
               weight-total]
              [(* (first sample)
                  weight-total)
               weight-total])))

(clj/defn update-weighted-sum
          [[weighted-sum weight-total] sample]
          (let [weight-total (+ weight-total
                                (exp (second sample)))]
            (if (vector? weighted-sum)
              [(mapv (fn [v1 v2]
                      (+ v1
                         (* v2
                            (exp (second sample)))))
                    weighted-sum
                    (first sample))
               weight-total]
              [(+ weighted-sum
                        (* (first sample)
                           (exp (second sample))))
               weight-total])))

; ===============================================
; =============== Bin Computation ===============
; ===============================================

(clj/defn compute-bins
          [samples bins]
          (loop [acc              (initialize-bins (first samples)
                                                   bins)
                 samples          (rest samples)]
            (if (empty? samples)
              (normalize-bins acc)
              (recur (update-bins acc
                                  (first samples))
                     (rest samples)))))

(clj/defn normalize-bins
          [[bins weight-total]]
          (if (vector? bins)
            (mapv (fn [bin]
                    (reduce (fn [acc k]
                                (assoc acc k (/ (get acc k)
                                                weight-total)))
                            bin
                            (keys bin)))
                 bins)
            (map (fn [k]
                   (assoc bins k (/ (get bins k)
                                    weight-total)))
                 (keys bins))))

(clj/defn initialize-bins
          [sample bins]
          (let [weight-total (exp (second sample))]
            (if (vector? (first sample))
              [(mapv (fn [v]
                       (assoc bins
                              v
                              weight-total))
                    (first sample))
               weight-total]
              [(assoc bins
                      (first sample)
                      weight-total)
               weight-total])))

(clj/defn update-bins
          [[bins weight-total] sample]
          (let [weight-total (+ weight-total
                                (exp (second sample)))]
            (if (vector? bins)
              [(mapv (fn [bin v2]
                       (assoc bin
                              v2
                              (+ (get bin v2)
                                 (exp (second sample)))))
                    bins
                    (first sample))
               weight-total]
              [(assoc bins
                      (first sample)
                      (+ (get bins (first sample))
                         (exp (second sample))))
               weight-total])))
