;; ## Advent of Code 2021
(ns coyotesqrl.2021.day1
  (:require [coyotesqrl.utils :as utils]))

;; ### Day 1
;; Input as long sequence
(def day1-input (->> (utils/input->seq "coyotesqrl/2021/day1-input.txt")
                     (map #(Long/parseLong %))))

;; #### Part 1
;; To do this, count the number of times a depth measurement increases from the previous measurement.
;; (There is no measurement before the first measurement.)
(->> day1-input
     (partition 2 1 [0])
     (filter (fn [[a b]] (< a b)))
     count)

;; #### Part 2
;; Instead, consider sums of a three-measurement sliding window.
;; Again considering the above example:
;; Start by comparing the first and second three-measurement windows.
;; The measurements in the first window are marked A (199, 200, 208);
;; their sum is 199 + 200 + 208 = 607. The second window is marked B (200, 208, 210);
;; its sum is 618. The sum of measurements in the second window is larger than the sum of the first,
;; so this first comparison increased.
;; Your goal now is to count the number of times the sum of measurements in this sliding window
;; increases from the previous sum. So, compare A with B, then compare B with C, then C with D,
;; and so on. Stop when there aren't enough measurements left to create a new three-measurement sum.
(->> day1-input
     (partition 3 1)
     (map #(apply + %))
     (partition 2 1 [0])
     (filter (fn [[a b]] (< a b)))
     count)
