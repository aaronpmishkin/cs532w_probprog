(ns inference.proposals
  "Implementation of Metropolis-Hastings."
  (:require [anglican.runtime       :as anglican]
            [foppl.graph            :as graph]
            [foppl.utils            :as utils]
            [clojure.core.matrix    :as m]
            [foppl.distributions    :refer :all]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def from-prior)
(def gibbs-from-prior)
(def gaussian-perturbation)
(def generic-gibbs)
(def blockwise-generic-gibbs)
(def retrieve-Q)
(def sample-to-vec)
(def vec-to-sample)

; ==============================================
; ============ Proposal Functions ==============
; ==============================================

(defn retrieve-Q
  [inf-method]
  (cond
    (= inf-method :prior)       from-prior
    (= inf-method :gfp)         gibbs-from-prior
    (= inf-method :gp)          gaussian-perturbation
    (= inf-method :gg)          generic-gibbs
    (= inf-method :bgg)         blockwise-generic-gibbs
    :else                       generic-gibbs))

(anglican/defdist from-prior
  [s G]
  (sample*
    [this]
    (graph/sample-from-consistent-joint G false))
  (observe*
    [this s-prime]
    (graph/score-assignment s-prime G false)))


(anglican/defdist gibbs-from-prior
  [v s G Sigma index-map]
  (sample*
    [this]
    (graph/sample-from-markov-blanket (first v) s G))
  (observe*
    [this s-prime]
    (graph/score-vertex s-prime (get G :P) (first v))))

(anglican/defdist gaussian-perturbation
  [v s G Sigma index-map]
  (sample*
    [this]
    (assoc s (first v) (anglican/sample* (anglican/normal (get s (first v))
                                                  (Math/sqrt (graph/marginalize-covariance Sigma [(get index-map (first v))]))))))
  (observe*
    [this s-prime]
    (anglican/observe* (anglican/normal (get s (first v))
                                        (Math/sqrt (graph/marginalize-covariance Sigma [(get index-map (first v))])))
                       (get s-prime (first v)))))

(anglican/defdist generic-gibbs
  [v s G Sigma index-map]
  (sample*
    [this]
    (if (graph/is-continuous? (first v) s G)
      (anglican/sample* (gaussian-perturbation v s G Sigma index-map))
      (anglican/sample* (gibbs-from-prior v s G Sigma index-map))))
  (observe*
    [this s-prime]
    (if (graph/is-continuous? (first v) s G)
      (anglican/observe* (gaussian-perturbation v s G Sigma index-map) s-prime)
      (anglican/observe* (gibbs-from-prior v s G Sigma index-map) s-prime))))



(anglican/defdist blockwise-generic-gibbs
  [V s G Sigma index-map]
  (sample*
    [this]
    (if (reduce (fn [acc v] (and acc (graph/is-continuous? v s G))) true V)
      (let [h (println Sigma)
            s-vec           (graph/sample-to-vec index-map s)
            h (println s-vec)
            s-prime-vec     (anglican/sample* (anglican/mvn s-vec Sigma))
            h (println "we got here")
            s-prime         (graph/vec-to-sample V index-map s s-prime-vec)]
        s-prime)
      (let [s-prime     (reduce (fn [acc v]
                                  (assoc acc v (anglican/sample* (gibbs-from-prior [v] s G Sigma index-map))))
                                s
                                V)]
        s-prime)))
  (observe*
    [this s-prime]
    (if (reduce (fn [acc v] (and acc (graph/is-continuous? v s-prime G))) true V)
      (anglican/observe* (anglican/mvn (graph/sample-to-vec index-map s) Sigma) (graph/sample-to-vec index-map s-prime))
      (reduce (fn [acc v] (+ acc (anglican/observe* (gibbs-from-prior [v] s G Sigma index-map) s-prime)))
              0
              V))))





;
