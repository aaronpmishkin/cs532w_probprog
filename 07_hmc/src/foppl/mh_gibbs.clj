(ns foppl.mh-gibbs
  "Implementation of Monte Carlo samplers for inference."
  (:require [anglican.runtime       :as anglican]
            [foppl.sampling         :as sampling]
            [foppl.scoring          :as scoring]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================


(def mh-in-gibbs)
(def mh-step)

; ==============================================
; =========== Inference Algorithms =============
; ==============================================

(defn mh-in-gibbs
  [Q G [s p-tilde] Sigma block-size index-map]
  (lazy-seq (cons s (mh-in-gibbs Q
                                 G
                                 (mh-step Q
                                          G
                                          s
                                          p-tilde
                                          Sigma
                                          index-map
                                          block-size)
                                 Sigma
                                 block-size
                                 index-map))))
(defn mh-step
  "Implements a generic Metropolis-hastings step."
  [Q G s p-tilde Sigma index-map block-size]
  (let [v                      (sampling/sample-n-prior-vertex-labels block-size G)
        s-prime                (anglican/sample* (Q v s G Sigma index-map))
        p-tilde-prime          (scoring/score-assignment s-prime G false)
        q                      (anglican/observe* (Q v s-prime G Sigma index-map)
                                                  s)
        q-prime                (anglican/observe* (Q v s G Sigma index-map)
                                                  s-prime)
        numer                  (+ p-tilde-prime
                                  q)
        denom                  (+ p-tilde
                                  q-prime)
        ratio                  (- numer
                                  denom)
        p                      (min 1 (Math/exp ratio))
        accept?                (anglican/sample* (anglican/flip p))]
    (if accept?
      [s-prime p-tilde-prime]
      [s p-tilde])))
