(ns foppl.proposals
  "Implementation of Metropolis-Hastings."
  (:require [anglican.runtime       :as anglican]
            [foppl.sampling         :as sampling]
            [foppl.scoring          :as scoring]
            [foppl.covariance       :as covariance]
            [foppl.utils            :as utils]
            [foppl.distributions    :refer :all]
            [clojure.core.matrix    :as m]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def from-prior)
(def gibbs-from-prior)
(def gaussian-perturbation)
(def generic-gibbs)
(def blockwise-generic-gibbs)
(def retrieve-Q)

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
    (sampling/sample-from-consistent-joint G false))
  (observe*
    [this s-prime]
    (scoring/score-assignment s-prime G false)))


(anglican/defdist gibbs-from-prior
  [v s G Sigma index-map]
  (sample*
    [this]
    (sampling/sample-from-markov-blanket (first v) s G))
  (observe*
    [this s-prime]
    (scoring/score-vertex s-prime (get G :P) (first v))))

(anglican/defdist gaussian-perturbation
  [v s G Sigma index-map]
  (sample*
    [this]
    (assoc s (first v) (anglican/sample* (anglican/normal (get s (first v))
                                                  (Math/sqrt (covariance/marginalize-covariance Sigma [(get index-map (first v))]))))))
  (observe*
    [this s-prime]
    (anglican/observe* (anglican/normal (get s (first v))
                                        (Math/sqrt (covariance/marginalize-covariance Sigma [(get index-map (first v))])))
                       (get s-prime (first v)))))

(anglican/defdist generic-gibbs
  [v s G Sigma index-map]
  (sample*
    [this]
    (if (get index-map (first v))
      (anglican/sample* (gaussian-perturbation v s G Sigma index-map))
      (anglican/sample* (gibbs-from-prior v s G Sigma index-map))))
  (observe*
    [this s-prime]
    (if (get index-map (first v))
      (anglican/observe* (gaussian-perturbation v s G Sigma index-map) s-prime)
      (anglican/observe* (gibbs-from-prior v s G Sigma index-map) s-prime))))



(anglican/defdist blockwise-generic-gibbs
  [V s G Sigma index-map]
  (sample*
    [this]
    (if (reduce (fn [acc v] (and acc (get index-map v))) true V)
      (let [s-vec           (utils/sample-to-vec index-map s)
            s-prime-vec     (anglican/sample* (anglican/mvn s-vec Sigma))
            s-prime         (utils/vec-to-sample V index-map s s-prime-vec)]
        s-prime)
      (let [s-prime     (reduce (fn [acc v]
                                  (assoc acc v (anglican/sample* (gibbs-from-prior [v] s G Sigma index-map))))
                                s
                                V)]
        s-prime)))
  (observe*
    [this s-prime]
    (if (reduce (fn [acc v] (and acc (get index-map v))) true V)
      (anglican/observe* (anglican/mvn (utils/sample-to-vec index-map s) Sigma) (utils/sample-to-vec index-map s-prime))
      (reduce (fn [acc v] (+ acc (anglican/observe* (gibbs-from-prior [v] s G Sigma index-map) s-prime)))
              0
              V))))





;
