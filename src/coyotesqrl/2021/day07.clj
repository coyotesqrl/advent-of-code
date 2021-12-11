;; ## Advent of Code 2021
;; ### Day 7
(ns coyotesqrl.2021.day07
  (:require [coyotesqrl.utils :as utils]
            [clojure.math.numeric-tower :as math]))

;; ##### Input
(def day7-input (utils/input->one-line-numeric "coyotesqrl/2021/day7-input.txt"))

;; #### Part 1
;; A giant whale has decided your submarine is its next meal, and it's much faster than you are.
;; There's nowhere to run!
;;
;; Suddenly, a swarm of crabs (each in its own tiny submarine - it's too deep for them otherwise)
;; zooms in to rescue you! They seem to be preparing to blast a hole in the ocean floor; sensors
;; indicate a massive underground cave system just beyond where they're aiming!
;;
;; The crab submarines all need to be aligned before they'll have enough power to blast a large
;; enough hole for your submarine to get through. However, it doesn't look like they'll be aligned
;; before the whale catches you! Maybe you can help?
;;
;; There's one major catch - crab submarines can only move horizontally.
;;
;; You quickly make a list of the horizontal position of each crab (your puzzle input). Crab
;; submarines have limited fuel, so you need to find a way to make all of their horizontal positions
;; match while requiring them to spend as little fuel as possible.

(defn test-point [p ls]
  (->> ls
       (map #(- p %))
       (map math/abs)
       (apply +)))

(defn test-points [l test-width tfn]
  (let [l (sort l)
        mid (quot 2 (+ (first l) (last l)))]
    (->> (for [i (range (- mid test-width) (+ mid test-width 1))]
           [(tfn i l) i])
         sort
         first)))

(time (test-points day7-input 400 test-point))

;; ---
;; #### Part 2
;; The crabs don't seem interested in your proposed solution. Perhaps you misunderstand crab
;; engineering?
;;
;; As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change
;; of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1,
;; the second step costs 2, the third step costs 3, and so on.

(defn test-point-incremental [p ls]
  (letfn [(gaussian-sum [len] (* (inc len) (/ len 2)))]
    (->> ls
         (map #(gaussian-sum (math/abs (- p %))))
         (apply +))))

(time (test-points day7-input 700 test-point-incremental))
