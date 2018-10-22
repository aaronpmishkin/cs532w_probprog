; @Author: aaronmishkin
; @Date:   18-10-13
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-20

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
  [Q G [s p-tilde] Sigma block-size index-map sampling-map scoring-map scoring-fn]
  (lazy-seq (cons (first s) (mh-in-gibbs Q
                                         G
                                         (mh-step Q
                                                  G
                                                  s
                                                  p-tilde
                                                  Sigma
                                                  index-map
                                                  block-size
                                                  sampling-map
                                                  scoring-map
                                                  scoring-fn)
                                         Sigma
                                         block-size
                                         index-map
                                         sampling-map
                                         scoring-map
                                         scoring-fn))))
(defn mh-step
  "Implements a generic Metropolis-hastings step."
  [Q G [s s-vec] p-tilde Sigma index-map block-size sampling-map scoring-map scoring-fn]
  (let [v                      (sampling/sample-n-prior-vertex-labels block-size G)
        [s-prime s-prime-vec]  (anglican/sample* (Q v s s-vec nil G Sigma index-map sampling-map scoring-map scoring-fn))
        p-tilde-prime          (scoring/score-assignment s-prime-vec scoring-fn)
        q                      (anglican/observe* (Q v s-prime s-prime-vec s G Sigma index-map sampling-map scoring-map scoring-fn)
                                                  s-vec)
        q-prime                (anglican/observe* (Q v s s-vec s-prime G Sigma index-map sampling-map scoring-map scoring-fn)
                                                  s-prime-vec)
        numer                  (+ p-tilde-prime
                                  q)
        denom                  (+ p-tilde
                                  q-prime)
        ratio                  (- numer
                                  denom)
        p                      (min 1 (Math/exp ratio))
        accept?                (anglican/sample* (anglican/flip p))]
    (if accept?
      [[s-prime s-prime-vec] p-tilde-prime]
      [[s s-vec] p-tilde])))
