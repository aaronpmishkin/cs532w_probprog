; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.utils
  "Utilities for all aspects of the FOPPL."
  (:require [clojure.walk           :as walk]))


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
(def topological-sort)
(def parse-density-expression)
(def parse-if-for-density)
(def unroll-args)
(def make-var-list)
(def pair-assignments)


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
          val    (if (or before
                         (not after))
                   -1
                   1)]
      val)))

(defn topological-sort
  [G]
  (let [comp-fn (make-topological-comparator (get G :A))
        V       (get G :V)]
    (apply vector (sort comp-fn V))))


; ============================================
; =========== Density Expressions ============
; ============================================

(defn parse-density-expression
  [e]
  (let [head (first e)
        body (rest e)]
       (cond
         (= 'if head)           (parse-if-for-density body)
         (= 'observe* head)     (first body))))

(defn parse-if-for-density
  [body]
  (let [[e1 e2 e3]      body
        E1              (eval e1)]
       (if E1
         (parse-density-expression e2)
         e3)))

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
