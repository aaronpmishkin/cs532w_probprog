; @Author: aaronmishkin
; @Date:   18-11-08
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-11-08

(ns hoppl.funcs
  "Useful functions implemented in the HOPPL."
  (:require [hoppl.core        :as core]
            [hoppl.utils       :as utils]))

; ===============================================
; =========== Pre-declare functions =============
; ===============================================

(def map)
(def reduce)
(def repeatedly)

; ===============================================
; ============== HOPPL Functions ================
; ===============================================

(core/defn map
  [func collection]
  (if (empty? collection)
    (list)
    (cons (func (first collection))
          (map (rest collection)))))

(core/defn reduce
  [func acc collection]
  (if (empty? collection)
    acc
    (reduce (func acc (first collection) (rest collection)))))

(core/defn repeatedly
   [n func]
   (if (= n 0)
     (list)
     (cons (func)
           (repeatedly (dec n)
                       func))))
