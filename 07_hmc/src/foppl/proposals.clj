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
  [s G sampling-map scoring-fn]
  (sample*
    [this]
    (sampling/sample-from-consistent-joint G sampling-map))
  (observe*
    [this s-prime-vec]
    (scoring/score-assignment s-prime-vec scoring-fn)))


(anglican/defdist gibbs-from-prior
  [v s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn]
  (sample*
    [this]
    (sampling/sample-from-markov-blanket (first v) s s-vec G sampling-map))
  (observe*
    [this s-prime-vec]
    (scoring/score-vertex s-prime-vec (first v) scoring-map)))

(anglican/defdist gaussian-perturbation
  [v s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn]
  (sample*
    [this]
    (let [s-prime       (assoc s (first v) (anglican/sample* (anglican/normal (get s (first v))
                                                                              (Math/sqrt (covariance/marginalize-covariance Sigma [(get index-map (first v))])))))
          s-prime-vec   (sampling/joint-sample-to-vec (get G :V) s-prime)]
      [s-prime s-prime-vec]))
  (observe*
    [this s-prime-vec]
    (anglican/observe* (anglican/normal (get s (first v))
                                        (Math/sqrt (covariance/marginalize-covariance Sigma [(get index-map (first v))])))
                       (get s-prime (first v)))))

(anglican/defdist generic-gibbs
  [v s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn]
  (sample*
    [this]
    (if (get index-map (first v))
      (anglican/sample* (gaussian-perturbation v s s-vec nil G Sigma index-map sampling-map scoring-map scoring-fn))
      (anglican/sample* (gibbs-from-prior v s s-vec nil G Sigma index-map sampling-map scoring-map scoring-fn))))
  (observe*
    [this s-prime-vec]
    (if (get index-map (first v))
      (anglican/observe* (gaussian-perturbation v s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn) s-prime-vec)
      (anglican/observe* (gibbs-from-prior v s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn) s-prime-vec))))



(anglican/defdist blockwise-generic-gibbs
  [V s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn]
  (sample*
    [this]
    (if (reduce (fn [acc v] (and acc (get index-map v))) true V)
      (let [s-vec           (utils/c-sample-to-vec index-map s)
            s-prime-vec     (anglican/sample* (anglican/mvn s-vec Sigma))
            s-prime         (utils/c-vec-to-sample V index-map s s-prime-vec)
            s-prime-vec     (sampling/joint-sample-to-vec (get G :V) s-prime)]
        [s-prime s-prime-vec])
      (let [s-prime     (reduce (fn [acc v]
                                  (assoc acc v (get (first (anglican/sample* (gibbs-from-prior [v] s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn))) v)))
                                s
                                V)
            s-prime-vec (sampling/joint-sample-to-vec (get G :V) s-prime)]
        [s-prime s-prime-vec])))
  (observe*
    [this s-prime-vec]
    (if (reduce (fn [acc v] (and acc (get index-map v))) true V)
      (anglican/observe* (anglican/mvn (utils/c-sample-to-vec index-map s) Sigma) (utils/c-sample-to-vec index-map s-prime))
      (reduce (fn [acc v] (+ acc (anglican/observe* (gibbs-from-prior [v] s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn) s-prime-vec)))
              0
              V))))





;
