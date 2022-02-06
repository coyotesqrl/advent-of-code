;; ## Advent of Code 2021
;; ### Day 21: Dirac Dice
(ns coyotesqrl.2021.day21
  (:require [coyotesqrl.utils :as utils]))

;; ##### Input
;; Player 1 starting position: 2
;; Player 2 starting position: 10

;; #### Part 1

(defn- roll [[v tot] amt]
  (let [v (mod (+ v amt) 10)
        tot (+ tot (if (zero? v) 10 v))]
    [v tot]))

(defn deterministic-game [p1 p2]
  (reduce (fn [a v]
            (let [player (if (even? (:rolls a)) :p1 :p2)
                  other-player (if (odd? (:rolls a)) :p1 :p2)
                  a (update a :rolls inc)
                  a (update a player #(roll % v))]
              (if (>= (second (player a)) 1000)
                (reduced (* 3 (:rolls a) (second (other-player a))))
                a)))
          {:rolls 0 :p1 [p1 0] :p2 [p2 0]}
          (range 6 Integer/MAX_VALUE 9)))

(utils/answer-block
  (deterministic-game 2 10))

;; ---
;; #### Part 2
;
; 4 - 444356092776315
; 8 - 341960390180808
(defn- steps-to-21 [s]
  ())

; Count of permutations (steps) for single player to get to between 21 and 21+9
; Same for second starting position
; For each winning step count, add losers for other player