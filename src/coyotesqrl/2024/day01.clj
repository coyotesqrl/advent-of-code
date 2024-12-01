;; ## Advent of Code 2024
;; ### [Day 1](https://adventofcode.com/2024/day/1)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2024/day01.clj)

(ns coyotesqrl.2024.day01
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
^{::clerk/visibility {:result :hide}}
(def day1-input
  (->> "coyotesqrl/2024/day1-input.txt"
       (utils/input->seq)))

^{::clerk/visibility {:result :hide}}
(defn parse-input [input]
  (let [input (map #(str/split % #"\s+") input)
        left (->> input (map first) (map parse-long) sort)
        right (->> input (map second) (map parse-long) sort)]
    {:left left :right right :combined (map vector left right)}))

^{::clerk/visibility {:result :hide}}
(defn part1-sum-diffs [{:keys [combined]}]
  (->> combined
       (map (fn [[l r]] (abs (- l r))))
       (reduce +)))

;; Example input
(part1-sum-diffs (parse-input '("3 4" "4 3" "2 5" "1 3" "3 9" "3 3")))

;; Result
(->> day1-input
     (parse-input)
     (part1-sum-diffs)
     (utils/answer-block))

;; ---

;; ---
;; #### Part 2

^{::clerk/visibility {:result :hide}}
(defn calculate-similarity-score [{:keys [left right]}]
  (reduce (fn [a v]
            (+ a (* v (count (filter #(= v %) right)))))
          0
          left))

;; Example input
(calculate-similarity-score (parse-input '("3 4" "4 3" "2 5" "1 3" "3 9" "3 3")))

;; Result
(->> day1-input
     (parse-input)
     (calculate-similarity-score)
     (utils/answer-block))
;; ---
