; @Author: aaronmishkin
; @Date:   18-09-26
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-09-26

(ns rubric-programs
  "FOPPL test/sample programs."
  (:require [foppl.core             :refer :all]
            [foppl.graph            :as graph]))


; PROGRAM #1

(def program1 (probabilistic-program
               (let [mu (sample (normal 1 (sqrt 5)))
                     sigma (sqrt 2)
                     lik (normal mu sigma)]
                   (observe lik 8)
                   (observe lik 9)
                  mu)))

(def G1 (graph/print-graph program1))

(graph/count-vertices G1)
(graph/count-edges G1)

; PROGRAM #2

(defn observe-data [_ data slope bias]
  (let [xn (first data)
        yn (second data)
        zn (+ (* slope xn) bias)]
    (observe (normal zn 1.0) yn)
    (rest (rest data))))

(def program2 (probabilistic-program
                   (let [slope (sample (normal 0.0 10.0))
                               bias  (sample (normal 0.0 10.0))
                               data (vector 1.0 2.1 2.0 3.9 3.0 5.3
                                            4.0 7.7 5.0 10.2 6.0 12.9)]
                     (loop 6 data observe-data slope bias)
                     (vector slope bias))))

(def G2 (graph/print-graph program2))

(graph/count-vertices G2)
(graph/count-edges G2)


; PROGRAM #3

(defn hmm-step [t states data trans-dists likes]
      (let [z (sample (get trans-dists
                           (last states)))]
        (observe (get likes z)
                 (get data t))
        (append states z)))

(def program3
     (probabilistic-program
       (let [data [0.9 0.8 0.7 0.0 -0.025 -5.0 -2.0 -0.1
                   0.0 0.13 0.45 6 0.2 0.3 -1 -1]
             trans-dists [(discrete [0.10 0.50 0.40])
                          (discrete [0.20 0.20 0.60])
                          (discrete [0.15 0.15 0.70])]
             likes [(normal -1.0 1.0)
                    (normal 1.0 1.0)
                    (normal 0.0 1.0)]
             states [(sample (discrete [0.33 0.33 0.34]))]]
         (loop 16 states hmm-step data trans-dists likes))))

(def G3 (graph/print-graph program3))

(graph/count-vertices G3)
(graph/count-edges G3)


; PROGRAM #4
(def program4
     (probabilistic-program
       (let [weight-prior (normal 0 1)
             W_0 (foreach 10 []
                   (foreach 1 [] (sample weight-prior)))
             W_1 (foreach 10 []
                   (foreach 10 [] (sample weight-prior)))
             W_2 (foreach 1 []
                   (foreach 10 [] (sample weight-prior)))

             b_0 (foreach 10 []
                   (foreach 1 [] (sample weight-prior)))
             b_1 (foreach 10 []
                   (foreach 1 [] (sample weight-prior)))
             b_2 (foreach 1 []
                   (foreach 1 [] (sample weight-prior)))

             x   (mat-transpose [[1] [2] [3] [4] [5]])
             y   [[1] [4] [9] [16] [25]]
             h_0 (mat-tanh (mat-add (mat-mul W_0 x)
                                    (mat-repmat b_0 1 5)))
             h_1 (mat-tanh (mat-add (mat-mul W_1 h_0)
                                    (mat-repmat b_1 1 5)))
             mu  (mat-transpose
                   (mat-tanh (mat-add (mat-mul W_2 h_1)
                                      (mat-repmat b_2 1 5))))]
         (foreach 5 [y_r y
                     mu_r mu]
            (foreach 1 [y_rc y_r
                        mu_rc mu_r]
               (observe (normal mu_rc 1) y_rc)))
         [W_0 b_0 W_1 b_1 W_2 b2])))

(def G4 (graph/print-graph program4))

(graph/count-vertices G4)
(graph/count-edges G4)
