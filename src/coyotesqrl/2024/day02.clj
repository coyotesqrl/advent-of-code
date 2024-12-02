;; ## Advent of Code 2024
;; ### [Day 2](https://adventofcode.com/2024/day/2)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2024/day02.clj)

(ns coyotesqrl.2024.day02
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
(def example-input
  '("7 6 4 2 1"
    "1 2 7 8 9"
    "8 6 4 4 1"
    "9 7 6 2 1"
    "1 3 2 4 5"
    "1 3 6 7 9"))

^{::clerk/visibility {:result :hide}}
(defn parse-reports [input]
  (->> input
       (map #(str/split % #"\s+"))
       (map (fn [l] (map #(parse-long %) l)))))

^{::clerk/visibility {:result :hide}}
(def day2-input
  (->> "coyotesqrl/2024/day2-input.txt"
       (utils/input->seq)
       (parse-reports)))

^{::clerk/visibility {:result :hide}}
(defn part1-safe-report [r]
  (->> r
       (filter #(or (apply > %) (apply < %)))
       (filter (fn [r]
                 (->> r
                      (partition 2 1)
                      (map (fn [[l r]] (abs (- l r))))
                      (every? #(<= 1 % 3)))))))

;; Result
(->> day2-input
     (part1-safe-report)
     (count)
     (utils/answer-block))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn inc-dec-dampened [r]
  (let [dampened-reports (map (fn [i]
                                (keep-indexed #(when (not= i %1) %2) r))
                              (range (count r)))]
    (->> dampened-reports
         (part1-safe-report))))

;; Result
(->> day2-input
     (map inc-dec-dampened)
     (filter not-empty)
     (count)
     (utils/answer-block))
