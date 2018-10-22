; @Author: aaronmishkin
; @Date:   18-10-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-12

(ns rubric-programs
  "Rubric programs for HW 06: Metropolis-within-Gibbs."
  (:require [foppl.core             :refer :all]
            [foppl.distributions    :refer :all]
            [foppl.graph            :as graph]
            [anglican.emit          :as emit]
            [anglican.core          :as c]
            [anglican.stat          :as stat]
            [anglican.runtime       :as anglican]))



; PROGRAM 1

(probabilistic-program :bgg 1 1
  (let [mu        (sample (normal 1 (sqrt 5)))
        sigma     (sqrt 2)
        lik       (normal mu sigma)]
    (observe lik 8)
    (observe lik 9)
    mu))


; PROGRAM 2

(defn observe-data [_ data slope bias]
  (let [xn (first data)
        yn (second data)
        zn (+ (* slope xn) bias)]
    (observe (normal zn 1.0) yn)
    (rest (rest data))))

(probabilistic-program :bgg 0.5 2
  (let [slope (sample (normal 0.0 10.0))
        bias  (sample (normal 0.0 10.0))
        data (vector 1.0 2.1 2.0 3.9 3.0 5.3
                     4.0 7.7 5.0 10.2 6.0 12.9)]
    (loop 6 data observe-data slope bias)
    (vector slope bias)))

; PROGRAM 3
(probabilistic-program :gg 0.05 1
  (let [data [1.1 2.1 2.0 1.9 0.0 -0.1 -0.05]
        likes (foreach 3 []
                (let [mu (sample (normal 0.0 10.0))
                      sigma (sample (gamma 1.0 1.0))]
                  (normal mu sigma)))
        pi (sample (dirichlet [1.0 1.0 1.0]))
        z-prior (discrete pi)
        zs      (foreach 7 [y data]
                  (let [z (sample z-prior)]
                    (observe (get likes z) y)
                    z))]
    (= (first zs)
       (second zs))))


; PROGRAM 4
(probabilistic-program :bgg 1 3
  (let [sprinkler true
        wet-grass true
        is-cloudy (sample (flip 0.5))

        is-raining (if (= is-cloudy true)
                      (sample (flip 0.8))
                      (sample (flip 0.2)))
        sprinkler-dist (if (= is-cloudy true)
                          (flip 0.1)
                          (flip 0.5))
        wet-grass-dist (if (and (= sprinkler true)
                                (= is-raining true))
                          (flip 0.99)
                          (if (and (= sprinkler false)
                                   (= is-raining false))
                            (flip 0.0)
                            (if (or (= sprinkler true)
                                    (= is-raining true))
                              (flip 0.9)
                              nil)))]

    (observe sprinkler-dist sprinkler)
    (observe wet-grass-dist wet-grass)
    is-raining))


; PROGRAM 5

(probabilistic-program :bgg 0.001 2
  (let [x (sample (normal 0 10))
        y (sample (normal 0 10))]
    (observe (dirac (+ x y)) 7)
    [x y]))
