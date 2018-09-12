(defquery poisson-trace [lambda]
  (let [k           1
        variate     (sample (uniform-continuous 0 1))
        p           variate
        L           (exp (- lambda))
        [k trace]           (loop [ik k
                                   ip p
                                   trace [variate]]
                              (if (<= ip L)
                                [(dec ik) trace]
                                (let [variate (sample (uniform-continuous 0 1))]
                                  (recur (inc ik)
                                         (* ip variate)
                                         (conj trace variate)))))]
    {:large? (> k 3)
     :k k
     :trace trace}))

;    (observe (flip 0.9) (> k 3)) ; add this line to condition on p(k > 3) = 0.9
