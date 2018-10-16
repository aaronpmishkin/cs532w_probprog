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
  [v s s-vec G Sigma s-prime-vec index-map]
  (sample*
    [this]
    (graph/sample-from-markov-blanket (first v) s G))
  (observe*
    [this s-prime]
    (graph/score-vertex s-prime (get G :P) (first v))))

(anglican/defdist gaussian-perturbation
  [v s s-vec G Sigma s-prime-vec index-map]
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
  [v s s-vec G Sigma s-prime-vec index-map]
  (sample*
    [this]
    (if (graph/is-continuous? (first v) s G)
      [(anglican/sample* (gaussian-perturbation v s s-vec G Sigma s-prime-vec index-map)) nil]
      [(anglican/sample* (gibbs-from-prior v s s-vec G Sigma s-prime-vec index-map)) nil]))
  (observe*
    [this s-prime]
    (if (graph/is-continuous? (first v) s G)
      (anglican/observe* (gaussian-perturbation v s s-vec G Sigma s-prime-vec index-map) s-prime)
      (anglican/observe* (gibbs-from-prior v s s-vec G Sigma s-prime-vec index-map) s-prime))))



(anglican/defdist blockwise-generic-gibbs
  [V s s-vec G Sigma s-prime-vec index-map]
  (sample*
    [this]
    (if (reduce 'and true (map (fn [v] (graph/is-continuous? v G))))
      (let [s-prime-vec     (anglican/sample* (anglican/mvn s-vec Sigma))
            h (println s-prime-vec)
            s-prime         (graph/vec-to-sample V index-map s s-prime-vec)]
        [s-prime s-prime-vec])
      (let [s-prime     (reduce (fn [acc v]
                                  (assoc acc v (anglican/sample* (gibbs-from-prior [v] s s-vec G Sigma s-prime-vec index-map))))
                                s
                                V)
            s-prime-vec     (graph/sample-to-vec index-map s-prime)]
        [s-prime s-prime-vec])))
  (observe*
    [V s-prime]
    (if (reduce 'and true (map (fn [v] (graph/is-continuous? v G))))
      (anglican/observe* (anglican/mvn s-vec Sigma) s-prime-vec)
      (reduce (fn [acc v] (+ acc (anglican/observe* (gibbs-from-prior [v] s s-vec G Sigma s-prime-vec index-map) s-prime)))))))





;
