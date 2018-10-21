; @Author: aaronmishkin
; @Date:   18-10-04
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-04

(ns auto-diff.backward
  "Core implementation of backward pass for auto-diff."
  (:require [anglican.runtime       :as anglican]
            [auto-diff.differs      :as differs]
            [auto-diff.utils        :as utils]))


; ================================================
; ============ Pre-declare Functions =============
; ================================================

(def backward)
(def backward-vertex)
(def backward-children)
(def compute-derivatives)
(def update-derivatives)


; ================================================
; ======== Entry Point for Backward Pass =========
; ================================================

; start the backward pass
(defn backward
  [vf G]
  (let [details             (utils/get-details vf G)
        details             (assoc details :adjoint 1)
        G                   (utils/set-details vf details G)]
    (backward-vertex vf G)))

; ================================================
; ================ Backward Pass =================
; ================================================

(defn backward-vertex
  [v G]
  (if (= 0
         (get (utils/get-details v G)
              :fanout))
    (let [A                 (get G :A)
          V                 (get G :V)
          children          (utils/get-children v A)
          details           (utils/get-details v G)
          e                 (get details :op)
          df                (get details :adjoint)
          dfs               (compute-derivatives e children V df)
          G                 (update-derivatives children dfs G)
          G-new             (backward-children children G)]
      G-new)
    G))


(defn backward-children
  [vertices graph]
  (loop [vs     vertices
         G      graph]
    (if (empty? vs)
      G
      (let [vi              (first vs)
            G-new           (backward-vertex vi G)]
        (recur (rest vs)
               G-new)))))


(defn compute-derivatives
  [e vs V df]
  (map (fn [vi]
         (* (differs/diff-expression e vi V)
            df))
       vs))


(defn update-derivatives
  [vertices derivatives graph]
  (loop [vs     vertices
         dfs    derivatives
         G      graph]
    (if (empty? vs)
      G
      (let [vi              (first vs)
            dfi             (first dfs)
            di              (utils/get-details vi G)
            fanout          (get di :fanout)
            dfi-prev        (get di :adjoint)
            di              (assoc di :adjoint (+ dfi dfi-prev))
            di              (assoc di :fanout (dec fanout))
            G-new           (utils/set-details vi di G)]
        (recur (rest vs)
               (rest dfs)
               G-new)))))
