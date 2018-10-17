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
            [foppl.utils          :as utils]
            [clojure.core.matrix  :as m]
            [foppl.distributions  :refer :all]
            [clojure.set            :refer [map-invert]]
            [foppl.evaluator      :as evaluator]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def create-graph)
(def merge-graphs)
(def merge-nested)
(def merge-graph-list)
(def create-edges)
(def print-graph)
(def count-vertices)
(def count-children)
(def count-edges)
(def score-vertex)
(def score-assignment)
(def is-prior?)
(def sample-vertex)
(def sample-from-graph)
(def sample-from-prior)
(def sample-from-joint)
(def sample-from-consistent-joint)
(def sample-from-markov-blanket)
(def sample-prior-vertex-label)
(def get-prior-vertices)
(def pull-n-samples)
(def sample-to-vec-labels)
(def sample-to-vec)
(def samples-to-matrix)
(def compute-empirical-covariance)
(def conditional-covariance)
(def sample-to-vec)
(def vec-to-sample)

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
        dist        (utils/parse-density-expression e)
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


; ============================================
; ================= Scoring ==================
; ============================================

(defn score-vertex
  [value-map P v]
  (let [e       (get P v)
        e       (walk/prewalk-replace value-map
                                      e)
        E       (utils/evaluate-density e)]
    E))

(defn score-assignment
  [value-map G sort?]
  (let [P       (get G :P)
        V       (if sort?
                  (utils/topological-sort G)
                  (get G :V))]
    (reduce (fn [acc v]
              (+ acc
                 (score-vertex value-map
                               P
                               v)))
            0
            V)))


; =============================================
; ================= Sampling ==================
; =============================================

(defn is-prior?
  [graph v]
  (nil? (get (get graph
                  :Y)
             v)))


(defn sample-vertex
  [graph value-map P v]
  (let [e       (get P v)
        E       (walk/prewalk-replace value-map
                              e)
        dist    (utils/parse-density-expression E)
        sample  (if (= dist 1)
                  nil
                  (anglican/sample* (eval dist)))]
    (if (seq? sample)
      (into [] sample)
      sample)))



(defn sample-from-graph
  [graph prior-only sort?]
  (let [P (get graph :P)
        V (if sort
            (utils/topological-sort graph)
            (get graph :V))]
    (reduce (fn [sample-map v]
              (if (or (not prior-only)
                      (is-prior? graph v))
                (assoc sample-map
                       v
                       (sample-vertex graph
                                      sample-map
                                      P
                                      v))
                sample-map))
            {}
            V)))

(defn ordered-sample-from-graph
  [graph prior-only]
  (let [P (get graph :P)
        V (get graph :V)]
    (reduce (fn [[acc sample-map] v]
              (if (or (not prior-only)
                      (is-prior? graph v))
                (let [sample (sample-vertex graph
                                            sample-map
                                            P
                                            v)]
                  [(conj acc sample) (assoc sample-map v sample)])

                [acc sample-map]))
            [[] {}]
            V)))

(defn sample-from-prior
  [graph sort?]
  (sample-from-graph graph
                     true
                     sort?))


(defn sample-from-joint
  [graph sort?]
  (sample-from-graph graph
                     false
                     sort?))


; ==============================================
; ============== Inference Utils ===============
; ==============================================

(defn sample-from-consistent-joint
  [G sort?]
  (let [Y                   (get G :Y)
        ks                  (keys Y)
        value-map           (sample-from-prior G sort?)]
    (reduce (fn [acc, k]
              (assoc acc
                     k
                     (get Y k)))
            value-map
            ks)))

(defn vec-sample-from-consistent-joint
  [G index-map]
  (let [value-map           (sample-from-consistent-joint G false)
        value-vec           (sample-to-vec index-map value-map)]
    [value-vec value-map]))


(defn sample-from-markov-blanket
  [v s G]
  (let [P                   (get G :P)
        v-sample            (sample-vertex G s P v)
        s-prime             (assoc s v v-sample)]
    s-prime))

(defn sample-prior-vertex-label
  [G]
  (let [V                   (get G :V)
        prior-V             (get-prior-vertices G V)
        num-vertices        (count prior-V)
        index               (anglican/sample* (anglican/uniform-discrete 0 num-vertices))
        v                   (nth prior-V index)]
    v))

(defn sample-n-prior-vertex-labels
  [n G]
  (loop [n      n
         labels []]
    (if (= n 0)
      labels
      (let [l           (sample-prior-vertex-label G)
            chosen?     (reduce (fn [acc v] (or acc (= v l)))
                                false
                                labels)]
        (if chosen?
          (recur n labels)
          (recur (dec n)
                 (conj labels l)))))))

; ==============================================
; =========== Empirical Covariance =============
; ==============================================

(defn pull-n-samples
  [G n]
  (loop [n       n
         samples []]
    (if (= n 0)
      samples
      (recur (dec n)
             (conj samples
                   (sample-from-joint G false))))))

(defn sample-to-vec-labels
  [s]
  (map first
       (into [] s)))

(defn sample-to-vec
  [s]
  (map second
       (into [] s)))

(defn samples-to-matrix
  [s-list]
  (m/matrix (map sample-to-vec s-list)))

(defn construct-index-map
  [labels]
  (loop [acc    {}
         labels labels
         index  0]
    (if (empty? labels)
      acc
      (recur (assoc acc (first labels) index)
             (rest labels)
             (inc index)))))

(defn remove-discrete-vars
  [s-matrix s labels index-map G]
  (let [continuous-indices  (reduce (fn [acc v]
                                      (if (is-continuous? v s G)
                                        (conj acc (get index-map v))
                                        acc))
                                    []
                                    labels)
        [index-map i]       (reduce (fn [[acc i] v]
                                        (if (is-continuous? v s G)
                                          [(assoc acc v i) (inc i)]
                                          [acc i]))
                                    [{} 0]
                                    labels)
        columns              (map (fn [index] (m/get-column s-matrix index))
                                  continuous-indices)
        s-matrix             (m/transpose (m/matrix columns))]
    [s-matrix index-map]))


(defn compute-empirical-covariance
  [G n]
  (let [V               (get G :V)
        samples         (repeatedly n (fn [] (ordered-sample-from-graph G false)))
        value-map       (second (first samples))
        samples         (map first samples)
        samples         (map (fn [x] (map (fn [y]
                                            (seq? y) 0 y)
                                          x))
                             samples)
        labels          V
        index-map       (construct-index-map labels)
        s-matrix        (m/matrix samples)
        [s-matrix
         index-map]     (remove-discrete-vars s-matrix value-map labels index-map G)
        n               (count (keys index-map))
        observed-vars   (map first (into [] (get G :Y)))
        k               (count (reduce (fn [acc v] (if (get index-map v)
                                                     (conj acc v)
                                                     acc))
                                       []
                                       observed-vars))
        index-map       (reduce (fn [acc v] (dissoc acc v))
                                index-map
                                observed-vars)
        cov             (if (> n 0)
                          (conditional-covariance (anglican/covariance s-matrix)
                                                  n
                                                  k)
                          1)]
    [cov index-map]))


(defn conditional-covariance
  [M n k]
  (let [Sigma11         (m/submatrix M 0 (- n k) 0 (- n k))
        Sigma22         (m/submatrix M (- n k) k (- n k) k)
        Sigma12         (m/submatrix M 0 (- n k) (- n k) k)
        Sigma22-inv     (m/inverse Sigma22)
        Sigma           (m/sub Sigma11
                               (m/mmul Sigma12 (m/mmul Sigma22-inv
                                                       (m/transpose Sigma12))))]
    Sigma))


(defn marginalize-covariance
  [Sigma indices]
  (if (= 1 (count indices))
     (let [z (m/mget Sigma (first indices)
                           (first indices))]
       z)
     nil))




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
