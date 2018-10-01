; @Author: aaronmishkin
; @Date:   18-09-29
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-29


(ns foppl.evaluator
  "Core implementation of the FOPPL compiler."
  (:require [foppl.utils            :as utils]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def partial-evaluate)
(def vector-expression?)
(def list-expression?)
(def map-expression?)
(def eval-count)
(def eval-conj)
(def eval-first)
(def eval-second)
(def eval-rest)
(def eval-nth)
(def eval-last)
(def eval-list-count)
(def eval-list-conj)
(def eval-list-first)
(def eval-list-second)
(def eval-list-rest)
(def eval-list-nth)
(def eval-list-last)
(def eval-cons)
(def eval-assoc)
(def eval-get)

; ===============================================
; ============= Vector Evaluators ===============
; ===============================================

(defn partial-evaluate
  [e]
  (let [head (first e)
        body (rest e)
        h    (println e)]
    (cond
      (vector-expression? e)
      (cond
        ; Partial Evaluators for VECTORS
        (= 'count  head)              (eval-count body)
        (= 'first  head)              (eval-first body)
        (= 'rest   head)              (eval-rest body)
        (= 'second head)              (eval-second body)
        (= 'conj   head)              (eval-conj body)
        (= 'nth    head)              (eval-nth body)
        (= 'get    head)              (eval-nth body)
        (= 'last   head)              (eval-last body)
        :else                         e)
      (map-expression? e)
      (cond
        ; Partial Evaluators for HASHMAPS
        (= 'assoc  head)              (eval-assoc body)
        (= 'get    head)              (eval-get body)
        :else                         e)
      (list-expression? e)
      (cond
      ; Partial Evaluators for LISTS
        (= 'cons   head)              (eval-cons body)
        (= 'count  head)              (eval-list-count body)
        (= 'first  head)              (eval-list-first body)
        (= 'rest   head)              (eval-list-rest body)
        (= 'second head)              (eval-list-second body)
        (= 'conj   head)              (eval-list-conj body)
        (= 'nth    head)              (eval-list-nth body)
        (= 'get    head)              (eval-list-nth body)
        (= 'last   head)              (eval-list-last body)
        :else                         e)
      :else  e)))


(defn vector-expression?
  [e]
  (vector? (second e)))

(defn list-expression?
  [e]
  (or (and (list? (second e))
           (= (first (second e))
              'list))
      (and (= (first e)
              'cons)
           (= (first (nth e 2))
              'list))))

(defn map-expression?
  [e]
  (map? (second e)))


; VECTORS

(defn eval-count
  [[e]]
  (count e))

(defn eval-conj
  [e]
  (conj (first e) (second e)))

(defn eval-first
  [[e]]
  (first e))

(defn eval-second
  [[e]]
  (second e))

(defn eval-rest
  [[e]]
  (into [] (rest e)))

(defn eval-nth
  [e]
  (nth (first e)
       (second e)))

(defn eval-last
  [[e]]
  (last e))

; LISTS

(defn eval-list-count
  [[e]]
  (dec (count e)))

(defn eval-list-conj
  [e]
  (conj (conj (rest (first e))
              (second e))
        'list))

(defn eval-list-first
  [[e]]
  (second e))

(defn eval-list-second
  [[e]]
  (nth e 2))

(defn eval-list-rest
  [[e]]
  (conj (rest (rest e))
        'list))

(defn eval-list-nth
  [e]
  (nth (rest (first e))
       (second e)))

(defn eval-list-last
  [[e]]
  (last e))

(defn eval-cons
  [e]
  (let [a       (first e)
        b       (second e)
        head    (first b)
        vals    (rest b)]
    (cons head (cons a vals))))

; MAPS

(defn eval-assoc
  [e]
  (assoc (first e)
         (second e)
         (last e)))

(defn eval-get
  [e]
  (let [a       (first e)
        b       (second e)]

    (if (map? a)
        (get a b)
        (eval-nth e))))
