; @Author: aaronmishkin
; @Date:   18-10-04
; @Email:  amishkin@cs.ubc.ca
; @Last modified by:   aaronmishkin
; @Last modified time: 18-10-04

(ns auto-diff.forward
  "Core implementation of forward evaluation for auto-diff."
  (:require [auto-diff.utils        :as utils]))


; ================================================
; ============ Pre-declare Functions =============
; ================================================


(def forward-expression)
(def forward-terminal)
(def forward-if)
(def forward-procedure)
(def forward-params)
(def return-expression)

; ===============================================
; ============= Forward Evaluation ==============
; ===============================================

(defn forward-expression
  [e G type]
  (cond
    (contains? (get G :S) e)        (return-expression e G)
    (not (list? e))                 (forward-terminal e G type)
    :else                           (let [head (first e)]
                                      (if (= 'if head)
                                        (forward-if e G type)
                                        (forward-procedure e G type)))))

(defn forward-terminal
  [e G type]
  (let [v                   (gensym "vertex_")
        E                   e
        [V S]               (utils/multiple-get G [:V :S])
        details             {:value E
                             :adjoint 0
                             :type type}
        V-new               (assoc V v details)
        S-new               (assoc S e v)
        G-new               (utils/multiple-insert G
                                                   [[:S S-new]
                                                    [:V V-new]])]
    [v E G-new]))

(defn forward-if
  [e G type]
  (let [[_ e1 e2 e3]        e
        [_ E1 _]            (forward-expression e1 G type)]
    (if E1
      (forward-expression e2 G type)
      (forward-expression e3 G type))))


(defn forward-procedure
  [e G type]
  (let [head                (first e)
        params              (rest e)
        p                   (gensym "vertex_")
        [Vs Es G]           (forward-params params G :internal)
        E                   (apply head Es)
        [V S A]             (utils/multiple-get G
                                                [:V :S :A])
        S-new               (assoc S
                                   e
                                   p)
        A-new               (utils/create-edges A
                                                p
                                                Vs)
        details             {:value E
                             :adjoint 0
                             :type type
                             :op (conj (apply list
                                              Vs)
                                       head)}
        V-new               (assoc V
                                   p
                                   details)
        G-new               (utils/multiple-insert G
                                                   [[:V V-new]
                                                    [:A A-new]
                                                    [:S S-new]])]
    [p E G-new]))

(defn forward-params
  [params graph type]
  (loop [es     params
         Vs     []
         Es     []
         G      graph]
    (if (empty? es)
      [Vs Es G]
      (let [ei          (first es)
            [vi Ei Gi]  (forward-expression ei
                                            G
                                            type)
            G-new       (utils/merge-graphs Gi
                                            G)]
        (recur (rest es)
               (conj Vs
                     vi)
               (conj Es
                     Ei)
               G-new)))))


(defn return-expression
  [e G]
  (let [S                   (get G :S)
        V                   (get G :V)
        v                   (get S e)
        details             (get V v)
        E                   (get details :value)]
    [v E G]))
