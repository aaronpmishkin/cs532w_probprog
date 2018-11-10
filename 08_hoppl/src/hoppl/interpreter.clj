; @Author: aaronmishkin
; @Date:   18-11-08
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-11-08

(ns hoppl.interpreter
  "Core implementation of the FOPPL compiler."
  (:require [anglican.runtime       :as anglican]
            [clojure.walk           :as walk]
            [hoppl.sugar            :as sugar]
            [hoppl.utils            :as utils]))



; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def desugar-program)
(def desugar-functions)
(def evaluate-expression)
(def evaluate-symbol)
(def evaluate-vector)
(def evaluate-map)
(def evaluate-fn)
(def evaluate-let)
(def evaluate-if)
(def evaluate-sample)
(def evaluate-observe)
(def evaluate-procedure)
(def call-procedure)

; =============================================
; ============ Anonymous Functions ============
; =============================================

(def anonymous-functions {})

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
; ============ Expression Evaluator ============
; ==============================================

; Notes:



(defn evaluate-expression
  [rho sigma loglik e]
  (if (not (seq? e))
    (cond
      (vector? e)                           (evaluate-vector rho sigma loglik e)
      (map? e)                              (evaluate-map rho sigma loglik e)
      :else                                 (evaluate-symbol rho sigma loglik e))
    (let [[head loglik]    (trampoline evaluate-expression rho sigma loglik (first e))
          e  (cons head (rest e))]
         (cond
           (= 'fn head)                     (evaluate-fn rho sigma loglik e)
           (= 'if head)                     (evaluate-if rho sigma loglik e)
           (= 'sample head)                 (evaluate-sample rho sigma loglik e)
           (= 'observe head)                (evaluate-observe rho sigma loglik e)
           :else                            (evaluate-procedure rho sigma loglik e)))))


; ===============================================
; ============ Individual Evaluators ============
; ===============================================

(defn evaluate-symbol
  [rho sigma loglik e]
  (fn [] [(get sigma e e) loglik]))


(defn evaluate-vector
  [rho sigma loglik e]
  (reduce (fn [[acc loglik] e]
            (let [[E loglik]    (trampoline evaluate-expression
                                            rho
                                            sigma
                                            loglik
                                            e)
                  acc       (conj acc E)]
              [acc loglik]))
          [[] loglik]
          e))


(defn evaluate-map
  [rho sigma loglik e]
  (reduce (fn [[acc loglik] k]
              (let [[E loglik]      (trampoline evaluate-expression
                                                rho
                                                sigma
                                                loglik
                                                (get e k))
                    acc             (assoc acc k E)]
                [acc loglik]))
          [{} loglik]
          (keys e)))


(defn evaluate-procedure
  [rho sigma loglik e]
  (let [head            (first e)
        params          (rest e)
        [Es loglik]     (reduce (fn [[Es loglik] ei]
                                    (let [[Ei loglik] (trampoline evaluate-expression
                                                                  rho
                                                                  sigma
                                                                  loglik
                                                                  ei)]
                                      [(conj Es Ei) loglik]))
                                [[] loglik]
                                params)]
       #(call-procedure rho
                        sigma
                        loglik
                        head
                        Es)))


(defn evaluate-fn
  [rho sigma loglik e]
  (let [[s args body]       e
        func-name           (gensym "func_")
        func-def            {:params args
                             :body   body}
        anons               (assoc anonymous-functions
                                   func-name
                                   func-def)]
    (def anonymous-functions anons)
    (fn [] [func-name loglik])))


(defn evaluate-if
  [rho sigma loglik e]
  (let [[s e1 e2 e3]    e
        [E1 loglik]     (trampoline evaluate-expression rho sigma loglik e1)]
    (if E1
      #(evaluate-expression rho sigma loglik e2)
      #(evaluate-expression rho sigma loglik e3))))



(defn evaluate-sample
  [rho sigma loglik e]
  (let [[s e1]          e
        [E1 loglik]     (trampoline evaluate-expression rho sigma loglik e1)
        S               (anglican/sample* E1)]
    (fn [] [S loglik])))


(defn evaluate-observe
  [rho sigma loglik e]
  (let [[s e1 e2]       e
        [E1 loglik]     (trampoline evaluate-expression rho sigma loglik e1)
        [E2 loglik]     (trampoline evaluate-expression rho sigma loglik e2)
        l3              (anglican/observe* E1 E2)
        loglik          (+ loglik l3)]
    (fn [] [E2 loglik])))


(defn call-procedure
  [rho sigma loglik head Es]
  (if (not (or (contains? rho head)
               (contains? anonymous-functions head)))
    [(apply (resolve head) Es) loglik]
    (let [fn-def    (get rho head (get anonymous-functions head))
          params    (get fn-def :params)
          body      (get fn-def :body)
          sigma     (utils/l-multiple-insert sigma
                                             params
                                             Es)]
      #(evaluate-expression rho sigma loglik body))))






;
