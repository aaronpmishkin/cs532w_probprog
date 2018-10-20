; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.utils
  "Utilities for all aspects of the FOPPL."
  (:require [clojure.walk           :as walk]
            [clojure.core.matrix    :as m]
            [clojure.set            :refer [map-invert]]))



; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def merge-vectors)
(def sanitize-symbol)
(def build-map)
(def multiple-insert)
(def symbol-replacer)
(def make-symbol-map)
(def replace-variables)
(def is-let-expression?)
(def get-let-assignment-var)
(def free?)
(def free-var-recorder)
(def get-free-vars)
(def make-topological-comparator)
(def make-observe-comparator)
(def topological-sort)
(def unroll-args)
(def make-var-list)
(def pair-assignments)
(def sample-to-vec-labels)
(def sample-to-vec)
(def vec-to-sample)

; ==============================================
; ============= General Utilities ==============
; ==============================================

(defn merge-vectors
  [v1 v2]
  (reduce conj v1 v2))

(defn sanitize-symbol
  [symbol]
  (keyword (gensym (str symbol "_"))))


(defn build-map
  [symbols values]
  (loop [acc {}
         syms symbols
         vals values]
    (if (empty? syms)
      acc
      (recur (assoc acc
                    (first syms)
                    (first vals))
             (rest syms)
             (rest vals)))))

(defn multiple-insert
  [map key-value-pairs]
  (reduce (fn [map [key value]]
            (assoc map key value))
          map
          key-value-pairs))


; =============================================
; ============= Symbol Replacers ==============
; =============================================

(defn symbol-replacer
  [symbol-map]
  (fn [symbol]
    (if (and (is-let-expression? symbol)
             (contains? symbol-map
                        (get-let-assignment-var symbol)))
      (let [nested-assignment-var (get-let-assignment-var symbol)]
       (assoc symbol-map
              nested-assignment-var
              nested-assignment-var)
       symbol)
      (if (contains? symbol-map symbol)
        (get symbol-map symbol)
        symbol))))


(defn make-symbol-map
  [symbol-map symbol]
  (assoc symbol-map
         symbol
         (sanitize-symbol symbol)))

(defn replace-variables
  [params compiled-params body]
  (walk/prewalk (symbol-replacer (build-map params
                                            compiled-params))
                body))

; ==============================================
; ============== Let Expressions ===============
; ==============================================


(defn is-let-expression?
  [e]
  (and (list? e)
       (= 'let
          (first e))))

(defn get-let-assignment-var
  [e]
  (first (second e)))


; =============================================
; ============== Free Variables ===============
; =============================================

(defn free?
  [e]
  (and (not (list? e))
       (not (keyword? e))
       (symbol? e)
       (and (try (not (resolve e))
                 (catch ClassCastException
                        e
                        true)))
       (not (resolve 'e))))

(defn free-var-recorder
  [rho]
  (fn [e]
   (if (and (free? e)
            (not (contains? rho e)))
       (do
         (def free-vars (conj free-vars e))
         e)
       e)))

(defn get-free-vars
  [rho E]
  (do
    (def free-vars #{})
    (walk/postwalk (free-var-recorder rho)
                   E)
    free-vars))

; =============================================
; =========== Topological Ordering ============
; =============================================

(defn make-topological-comparator
  [A]
  (fn [v1 v2]
    (let [before (not (nil? (get (get A v1)
                                 v2)))
          after  (not (nil? (get (get A v2)
                                 v1)))
          val    (cond before -1
                       after   1
                       :else   0)]

      val)))

(defn make-observe-comparator
  [Y]
  (fn [v1 v2]
    (let [before (not (nil? (get Y v2)))
          after  (not (nil? (get Y v1)))
          val    (cond before -1
                       after   1
                       :else   0)]

      val)))

(defn topological-sort
  [G]
  (let [comp-fn         (make-topological-comparator (get G :A))
        observe-comp-fn (make-observe-comparator (get G :Y))
        V       (get G :V)]
    (apply vector (sort observe-comp-fn
                        (sort comp-fn
                              (sort comp-fn V))))))



; ==================================================
; ============= De-sugaring Utilities ==============
; ==================================================

(defn unroll-args
  [start args]
  (apply list
         (reduce conj
                 start
                 args)))

(defn make-var-list
  [expressions]
  (reduce (fn [acc e]
            (conj acc
                  (gensym "newvar_")))
          []
          expressions))


(defn pair-assignments
  [assignments]
  (loop [acc []
         vec assignments]
    (if (empty? vec)
      acc
      (recur (conj acc
                   [(first vec) (second vec)])
             (rest (rest vec))))))


; ===================================================
; =============== Sampling Utilities ================
; ===================================================

(defn sample-to-vec-labels
  [s]
  (map first
       (into [] s)))

(defn sample-to-vec
  [index-map s-prime]
  (let [value-map   (map-invert index-map)
        ks          (keys value-map)]
    (loop [k           0
           s-prime-vec []]
      (if (= k (count ks))
        s-prime-vec
        (recur (inc k)
               (conj s-prime-vec
                     (get s-prime (get value-map k))))))))

(defn vec-to-sample
  [V index-map s vec-sample]
  (loop [V      V
         s-new  s]
    (if (empty? V)
      s-new
      (recur (rest V)
             (assoc s-new
                    (first V)
                    (nth vec-sample
                         (get index-map (first V))))))))
