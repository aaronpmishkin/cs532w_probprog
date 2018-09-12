(def start-x -5)
(def start-y 8.25)
(def bin-x 15)
(def bin-y 0)

;; To do this we've provided scaffolding
;; that can be modified to achieve
;; your objective:
(with-primitive-procedures
  [create-world simulate-world balls-in-box]
  (defquery arrange-bumpers []
    (let [num-bumpers (sample (uniform-discrete 1 2))    ; we consider a number of bumbers in [1 10]
          bumper-positions (repeatedly num-bumpers
                                       (fn [] [(sample (uniform-continuous start-x ; sample x-location
                                                                    bin-x))
                                               (sample (uniform-continuous bin-y   ; sample y-location
                                                                           start-y))]))

          ;; Code to simulate the world:
          world (create-world bumper-positions)
          end-world (simulate-world world)
          balls (:balls end-world)
          ;; How many balls entered the box?
          num-balls-in-box (balls-in-box end-world)
          p (/ num-balls-in-box
                (count balls))]
      (observe (binomial (count balls)
                         p)
               (count balls))            ; We want all of the balls to be in the basket
      {:balls balls                  ; so we define our observation as n = 10
       :num-balls-in-box num-balls-in-box
       :bumper-positions bumper-positions})))
