; @Author: aaronmishkin
; @Date:   18-11-08
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-11-08

(ns hoppl.utils
  "General utilities for the HOPPL language."
  (:require [clojure.walk           :as walk]))


(defn p-multiple-insert
    [map key-value-pairs]
    (reduce (fn [map [key value]]
              (assoc map key value))
            map
            key-value-pairs))

(defn l-multiple-insert
    [map keys values]
    (loop [ks   keys
           Es   values
           acc  map]
      (if (empty? ks)
        acc
        (recur (rest ks)
               (rest Es)
               (assoc acc
                      (first ks)
                      (first Es))))))
