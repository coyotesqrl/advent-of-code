;; ## Advent of Code 2023
;; ### [Day 6](https://adventofcode.com/2023/day/6)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day06.clj)

(ns coyotesqrl.2023.day06
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (let [[durations records] (->> "coyotesqrl/2023/day6-input.txt"
                                 (utils/input->seq)
                                 (map #(str/split % #":"))
                                 (map second)
                                 (map utils/extract-numbers))]
    (zipmap durations records)))

;; ##### Process races
^{::clerk/visibility {:result :hide}}
(defn winning-race-count [duration record]
  (count
   (filter #(> % record)
           (for [i (range 1 duration)]
             (* i (- duration i))))))

;; ##### Solve
(->> part1-input
     (map (fn [[d r]] (winning-race-count d r)))
     (apply *)
     (utils/answer-block))

;; ---
;; #### Part 2
(let [[duration record] (->> "coyotesqrl/2023/day6-input.txt"
                             (utils/input->seq)
                             (map #(str/split % #":"))
                             (map second)
                             (map #(str/replace % #"\s" ""))
                             (map parse-long))]
  (utils/answer-block (winning-race-count duration record)))
