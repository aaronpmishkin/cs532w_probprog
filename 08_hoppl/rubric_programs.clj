; @Author: aaronmishkin
; @Date:   18-11-08
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-11-08

(ns rubric-programs
  "Core implementation of the HOPPL."
  (:require [anglican.runtime       :as anglican]
            [hoppl.core             :refer :all]
            [hoppl.distributions    :refer :all]))


(defn until-success
  [p n dist]
  (if (sample dist)
    n
    (until-success p (+ n 1) dist)))

(probabilistic-program 100
  (let [p 0.01
        dist (flip p)]
       (until-success p 0 dist)))

(defn marsaglia-normal
  [mean var]
  (let [d (uniform-continuous -1.0 1.0)
        x (sample d)
        y (sample d)
        s (+ (* x x ) (* y y))]
       (if (< s 1)
         (+ mean (* (sqrt var)
                    (* x (sqrt (* -2 (/ (log s) s))))))
         (marsaglia-normal mean var))))

(probabilistic-program
  (let [mu (marsaglia-normal 1 5)
        sigma (sqrt 2)
        lik (normal mu sigma)]
    (observe lik 8)
    (observe lik 9)
    mu))

(probabilistic-program
  (let [observations [0.9 0.8 0.7 0.0 -0.025 -5.0 -2.0 -0.1 0.0 0.13 0.45 6 0.2 0.3 -1 -1]
        init-dist (discrete [1.0 1.0 1.0])
        trans-dists {0 (discrete [0.1 0.5 0.4])
                     1 (discrete [0.2 0.2 0.6])
                     2 (discrete [0.15 0.15 0.7])}
        obs-dists {0 (normal -1 1)
                   1 (normal 1 1)
                   2 (normal 0 1)}
        (reduce
          (fn [states obs]
            (let [state (sample (get trans-dists
                                     (peek states)))]
              (observe (get obs-dists state) obs)
              (conj states state)))
          [(sample init-dist)]
          observations)]))
