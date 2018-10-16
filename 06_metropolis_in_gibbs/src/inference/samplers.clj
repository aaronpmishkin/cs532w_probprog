(ns inference.samplers
  "Implementation of Monte Carlo samplers for inference."
  (:require [anglican.runtime       :as anglican]
            [foppl.utils            :as utils]
            [foppl.graph            :as graph]
            [clojure.core.matrix    :as m]))


; ===============================================
; =========== Pre-declare functions =============
; ===============================================


(def mh-in-gibbs)
(def mh-step)

; ==============================================
; =========== Inference Algorithms =============
; ==============================================

(defn mh-in-gibbs
  ([Q G Sigma scale block-size index-map]
   (let [Sigma                   (m/mul Sigma scale)
         h                       (println "starting mh" G)
         [s-vec s-start]         (graph/vec-sample-from-consistent-joint G index-map)
         p-tilde-start           (graph/score-assignment s-start
                                                         G
                                                         false)]
     (mh-in-gibbs Q G s-start p-tilde-start Sigma block-size index-map)))
  ([Q G s p-tilde Sigma block-size index-map]
   (let [v                                  (graph/sample-n-prior-vertex-labels block-size G)
         [s-next p-tilde-next]   (mh-step Q
                                                     G
                                                     v
                                                     s
                                                     p-tilde
                                                     Sigma
                                                     index-map)]
     (lazy-seq (cons s-next (mh-in-gibbs Q G s-next p-tilde-next Sigma block-size index-map))))))


(defn mh-step
  "Implements a generic Metropolis-hastings step."
  [Q G v s p-tilde Sigma index-map]
  (let [s-prime                (anglican/sample* (Q v s G Sigma index-map))
        p-tilde-prime          (graph/score-assignment s-prime G false)
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
