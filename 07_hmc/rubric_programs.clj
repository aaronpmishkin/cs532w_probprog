; @Author: aaronmishkin
; @Date:   18-10-12
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-12

(ns rubric-programs
  "Rubric programs for HW 07: HMC."
  (:require [foppl.core             :refer :all]
            [foppl.distributions    :refer :all]
            [foppl.graph            :as graph]
            [anglican.emit          :as emit]
            [anglican.core          :as c]
            [anglican.stat          :as stat]
            [anglican.runtime       :as anglican]))

(probabilistic-program :hmc 1 0.1
  (let [mu (sample (normal 1 (sqrt 5)))
           sigma (sqrt 2)
             lik (normal mu sigma)]
       (observe lik 8)
       (observe lik 9)
       mu))

(defn observe-data [_ data slope bias]
  (let [xn (first data)
        yn (second data)
        zn (+ (* slope xn) bias)]
    (observe (normal zn 1.0) yn)
    (rest (rest data))))

(probabilistic-program :hmc 1 0.1
  (let [slope (sample (normal 0.0 10.0))
        bias  (sample (normal 0.0 10.0))
        data (vector 1.0 2.1 2.0 3.9 3.0 5.3
                     4.0 7.7 5.0 10.2 6.0 12.9)]
    (loop 6 data observe-data slope bias)
    (vector slope bias)))


(probabilistic-program :hmc 1 0.1
  (let [x (sample (normal 0 10))
        y (sample (normal 0 10))]
    (observe (dirac (+ x y)) 7)
    [x y]))


(def n_iters 100)

(loop [i        n_iters
       x-avg    [0 0]
       y-avg    [0 0]]
  (if (= i 0)
    [[(/ (nth x-avg 0) n_iters) (/ (nth x-avg 1) n_iters)]
     [(/ (nth y-avg 0) n_iters) (/ (nth y-avg 1) n_iters)]]
    (let [result
                     (probabilistic-program :hmc 0.1 0.1
                       (let [x (sample (normal 0 10))
                             y (sample (normal 0 10))]
                         (observe (dirac (+ x y)) 7)
                         [x y]))]
      (recur
             (dec i)
             [(+ (first x-avg) (first (get result :mean))) (+ (second x-avg) (first (get result :variance)))]
             [(+ (first y-avg) (second (get result :mean))) (+ (second y-avg) (second (get result :variance)))]))))
