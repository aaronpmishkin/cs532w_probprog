; @Author: aaronmishkin
; @Date:   18-09-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-12

(ns foppl.graph
  "Core graph operations for the FOPPL language."
  (:require [anglican.runtime     :as anglican]
            [anglican.runtime     :refer [observe*]]
            [clojure.walk         :as walk]
            [foppl.distributions  :refer :all]
            [foppl.scoring        :as scoring]
            [foppl.utils          :as utils]
            [foppl.evaluator      :as evaluator]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================


(def create-graph)
(def merge-graphs)
(def merge-nested)
(def merge-graph-list)
(def create-edges)
(def is-continuous?)
(def is-prior?)
(def get-prior-vertices)
(def print-graph)
(def count-vertices)
(def count-children)
(def count-edges)
(def score-vertex)
(def score-assignment)
(def parse-density-expression)
(def parse-if-for-density)
(def wrap-score-expression)
(def build-score-function)
(def sum-score-expressions)
(def build-complete-scoring-fn)
(def build-score-map)
(def build-sampling-function)
(def build-sampling-map)

; =============================================
; ============== Graph Creation ===============
; =============================================

(defn create-graph
  []
  {:V []
   :A {}
   :P {}
   :Y {}})


(defn merge-graphs
  [G1 G2]
  (let [V (utils/merge-vectors (get G1 :V)
                         (get G2 :V))
        A (merge-nested (get G1 :A)
                        (get G2 :A))
        P (merge (get G1 :P)
                 (get G2 :P))
        Y (merge (get G1 :Y)
                 (get G2 :Y))]
    {:V V
     :A A
     :P P
     :Y Y}))

(defn merge-nested
  [map1 map2]
  (let [new-map (merge map1 map2)
        keys    (keys new-map)]
    (loop [map      new-map
           keys     keys]
      (if (empty? keys)
        map
        (recur (assoc map
                      (first keys)
                      (merge (get map1
                                  (first keys)
                                  {})
                             (get map2
                                  (first keys)
                                  {})))
               (rest keys))))))

(defn merge-graph-list
  [graphs]
  (reduce merge-graphs
          (first graphs)
          (rest graphs)))

(defn create-edges
  [A parents v]
  (reduce (fn [acc p]
            (assoc acc
                   p
                   (assoc (get acc p)
                          v
                          true)))
          A
          parents))

; =============================================
; ========== Basic Graph Operations ===========
; =============================================

(defn is-continuous?
  [v s G]
  (let [P           (get G :P)
        e           (get P v)
        dist        (parse-density-expression e)
        dist        (evaluator/partial-evaluate (walk/prewalk-replace s dist))
        head        (first dist)]
    (cond
        (= head 'binomial)           false
        (= head 'dirac)              true
        (= head 'flip)               false
        (= head 'beta)               true
        (= head 'normal)             true
        (= head 'binomial)           false
        (= head 'uniform-continuous) true
        (= head 'discrete)           false
        (= head 'dirichlet)          false
        (= head 'gamma)              false
        :else                        false)))


(defn is-prior?
  [graph v]
  (nil? (get (get graph
                  :Y)
             v)))

(defn get-prior-vertices
  [G V]
  (filter (fn [v] (is-prior? G v))
          V))

(defn print-graph
  [graph]
  (if (string? graph)
    (let [val   (read-string graph)
          h     (println val)
          G     (if (vector? val)
                    (second val)
                    val)]
      (println G)
      G)
    (do
      (println graph)
      graph)))

(defn count-vertices
  [graph]
  (count (get graph :V)))

(defn count-children
  [child-map]
  (reduce (fn [acc dict-pair]
            (if (last dict-pair)
              (inc acc)
              acc))
          0
          (seq child-map)))

(defn count-edges
  [graph]
  (reduce (fn [acc vertex]
            (+ acc
               (count-children (last vertex))))
          0
          (seq (get graph :A))))


; =============================================
; ===== Density and Sampler Constructors ======
; =============================================

(defn parse-density-expression
  [e]
  (let [head (first e)
        body (rest e)]
       (cond
         (= 'if head)                            (parse-if-for-density body)
         (= 'anglican.runtime/observe* head)     (first body))))

(defn parse-if-for-density
  [body]
  (let [[e1 e2 e3]      body
        E1              (eval e1)]
       (if E1
         (parse-density-expression e2)
         e3)))

(defn wrap-score-expression
  [expression]
  `(~'if (~'= ~expression 1)
         0
         ~expression))

(defn build-score-function
  [v G]
  (let [V           (get G :V)
        P           (get G :P)
        density-ex  (wrap-score-expression (get P v))]
    `(~'fn [[~@V]]
           ~density-ex)))

(defn sum-score-expressions
  [V P]
  (if (= (count V)
         2)
    (list '+
          (wrap-score-expression (get P (first V)))
          (wrap-score-expression (get P (second V))))
    (list '+
          (wrap-score-expression (get P (first V)))
          (sum-score-expressions (rest V)
                                 P))))

(defn build-complete-scoring-fn
  [G]
  (let [V           (get G :V)
        P           (get G :P)
        density-ex  (sum-score-expressions V P)]
    (eval `(~'fn [~@V]
                 ~density-ex))))

(defn build-quoted-scoring-fn
  [G]
  (let [V           (get G :V)
        P           (get G :P)
        density-ex  (sum-score-expressions V P)]
    `(~'fn [~@V]
           ~density-ex)))


(defn build-scoring-map
  [G]
  (let [V           (get G :V)]
    (reduce (fn [acc v]
              (assoc acc v (eval (build-score-function v G))))
            {}
            V)))

(defn build-sampling-function
  [v G]
  (let [V               (get G :V)
        P               (get G :P)
        density-ex      (parse-density-expression (get P v))]
    `(~'fn [[~@V]]
           (anglican/sample* ~density-ex))))


(defn build-sampling-map
  [G]
  (let [V           (get G :V)]
    (reduce (fn [acc v]
              (assoc acc v (eval (build-sampling-function v G))))
            {}
            V)))











;
