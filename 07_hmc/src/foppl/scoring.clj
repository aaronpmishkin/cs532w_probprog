; @Author: aaronmishkin
; @Date:   18-10-13
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-20


(ns foppl.scoring
  "Core graph operations for the FOPPL language."
  (:require [anglican.runtime     :as anglican]
            [clojure.walk         :as walk]
            [foppl.distributions  :refer :all]
            [foppl.utils          :as utils]))



; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def score-vertex)
(def score-assignment)

; ============================================
; ================= Scoring ==================
; ============================================

(defn score-vertex
  [s-vec v scoring-map]
  (let [scoring-fn (get scoring-map v)
        E          (scoring-fn s-vec)]
    E))

(defn score-assignment
  [s-vec complete-scoring-fn]
  (apply complete-scoring-fn s-vec))
