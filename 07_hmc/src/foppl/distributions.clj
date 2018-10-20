; @Author: aaronmishkin
; @Date:   18-10-14
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-14

(ns foppl.distributions
  "Core implementation of the FOPPL language grammar."
  (:require [anglican.runtime       :as anglican]))


; ===============================================
; ================ Distributions ================
; ===============================================

(def flip anglican/flip)
(def binomial anglican/binomial)
(def beta anglican/beta)
(def normal anglican/normal)
(def uniform-continuous anglican/uniform-continuous)
(def discrete anglican/discrete)
(def dirichlet anglican/dirichlet)
(def gamma anglican/gamma)


; Relaxed dirac distribution.

(def dirac-sigma 0.001)

(anglican/defdist dirac
  "Simple implementation of Dirac delta distribution."
  [x]
  (sample*  [this] x)
  (observe* [this value]
            (anglican/observe* (normal x dirac-sigma) value)))



; (anglican/defdist dirac
;   "Simple implementation of Dirac delta distribution."
;   [x]
;   (sample*  [this] x)
;   (observe* [this value]
;             (Math/log
;                       (if (= x value)
;                           1
;                           0))))
