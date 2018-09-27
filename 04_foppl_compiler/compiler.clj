; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.compiler
  "Core implementation of the FOPPL compiler."
  (:require [anglican.runtime       :as anglican]
            [clojure.walk           :as walk]
            [foppl.sugar            :as sugar]
            [foppl.utils            :as utils]
            [foppl.graph            :as graph]
            [foppl.graph-examples   :as examples]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def desuger-program)
(def desugar-functions)
(def compile-expression)
(def compile-let)
(def compile-procedure)
(def compile-if)
(def compile-sample)
(def compile-observe)
(def score)
(def try-get-symbol-value)
(def try-run-procedure)
(def deconstruct-map)
(def deconstruct-vector)

; =============================================
; ============== Sugar Removers ===============
; =============================================

(defn desugar-program
  [rho e]
  (let [rho (into {}
                  (desugar-functions rho))
        e   (walk/postwalk sugar/desugar-expression
                           e)]
    [rho e]))


(defn desugar-functions
  [rho]
  (reduce (fn [acc func-def]
            (let [body       (get (second func-def) :body)
                  body       (sugar/desugar-expression body)
                  func-def   [(first func-def) (assoc (second func-def)
                                                      :body
                                                      body)]]
              (conj acc func-def)))
          []
          (apply vector rho)))

; ==============================================
; ============ Expression Compiler =============
; ==============================================

(defn compile-expression
  [rho phi e]
  (if (not (list? e))
    (cond
      (vector? e)       (compile-expression rho
                                            phi
                                            (deconstruct-vector e))
      (map? e)          (compile-expression rho
                                            phi
                                            (deconstruct-map e))
      :else             [e (graph/create-graph)])
    (let [exp (try-get-symbol-value (first e))]
      (cond
        (= 'let exp)    (compile-let rho phi e)
        (= 'if exp)     (compile-if rho phi e)
        (= 'sample exp) (compile-sample rho phi e)
        (= 'observe exp)(compile-observe rho phi e)
        :else           (compile-procedure rho phi e)))))


; ===============================================
; ============ Individual Compilers =============
; ===============================================

(defn compile-let
  [rho phi [_ [v1 e1] e2]]
  (let [[E1 G1]         (compile-expression rho phi e1)
        e2              (walk/prewalk (utils/symbol-replacer {v1 E1})
                                      e2)
        [E2 G2]         (compile-expression rho phi e2)
        G               (graph/merge-graphs G1 G2)]
    [E2 G]))


(defn compile-procedure
  [rho phi e]
  (let [exp             (first e)
        params          (rest e)
        compiled-params (map (fn [ei]
                               (compile-expression rho
                                                   phi
                                                   ei))
                             params)
        Vs              (map first compiled-params)
        Gs              (map second compiled-params)
        [E G]           (try-run-procedure rho
                                       phi
                                       exp
                                       Vs)
        G               (graph/merge-graph-list (conj Gs G))]
    [E G]))


(defn compile-if
  [rho phi e]
  (let [[_ e1 e2 e3]  e
        [E1 G1]         (compile-expression rho phi e1)
        phi2            (list 'and phi E1)
        phi3            (list 'and phi (list 'not E1))
        h (println phi3)
        [E2 G2]         (compile-expression rho phi2 e2)
        [E3 G3]         (compile-expression rho phi3 e3)
        G               (graph/merge-graph-list [G1 G2 G3])
        E               (list 'if E1 E2 E3)]
    [E G]))


(defn compile-sample
  [rho phi e]
  (let [[_ e]   e
        [E G]           (compile-expression rho phi e)
        v               (gensym "vertex_")
        V               (get G :V)
        Z               (utils/get-free-vars rho E)
        A-new           (graph/create-edges (get G :A)
                                            Z
                                            v)
        V-new           (conj V v)
        F               (score E v)
        P-new           (assoc (get G :P)
                               v
                               F)
        G-new           (utils/multiple-insert G
                                               [[:A A-new]
                                                [:V V-new]
                                                [:P P-new]])]
    [v G-new]))


(defn compile-observe
  [rho phi e]
  (let [[_ e1 e2]   e
        [E1 G1]         (compile-expression rho phi e1)
        [E2 G2]         (compile-expression rho phi e2)
        G               (graph/merge-graphs G1 G2)
        v               (gensym "vertex_")
        F1              (score E1 v)
        F               (list 'if phi F1 1)
        Z               (utils/get-free-vars rho E1)
        A-new           (graph/create-edges (get G :A)
                                            Z
                                            v)
        V-new           (conj (get G :V)
                              v)
        P-new           (assoc (get G :P)
                               v
                               F)
        Y-new           (assoc (get G :Y)
                               v
                               E2)
        G-new           (utils/multiple-insert G
                                               [[:A A-new]
                                                [:V V-new]
                                                [:P P-new]
                                                [:Y Y-new]])]
    [E2 G-new]))




; ===============================================
; ============== HELPER Functions ===============
; ===============================================

(defn score
  [E v]
  (list 'observe* E v))

(defn try-get-symbol-value
  [s]
  (try (eval s)
       (catch RuntimeException
              e
              s)))


(defn try-run-procedure
  [rho phi exp Vs]
  (if (contains? rho
                 exp)
    (let [fn-def (get rho exp)
          params (get fn-def :params)
          body   (get fn-def :body)
          body   (utils/replace-variables params
                                          Vs
                                          body)]

      (compile-expression rho phi body))
    (let [[exp G]   (compile-expression rho phi exp)
          E1         (conj Vs exp)
          E2         (try (eval E1)
                         (catch RuntimeException
                                e
                                E1))
          ; Something clever here... hehehehe
          E3         (try (eval E2)
                         (catch RuntimeException
                                e
                                E1))
          G-new     (graph/create-graph)
          G         (graph/merge-graphs G G-new)]
      [E3 G])))

(defn deconstruct-map
  [e]
  (conj (apply list
               (reduce (fn [acc pair]
                         (conj (conj acc
                                     (first pair))
                               (second pair)))
                       []
                       (apply list e)))
        'hash-map))

(defn deconstruct-vector
  [e]
  (conj (apply list e) 'vector))
