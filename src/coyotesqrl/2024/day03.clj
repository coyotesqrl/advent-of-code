;; ## Advent of Code 2024
;; ### [Day 3](https://adventofcode.com/2024/day/3)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2024/day03.clj)

(ns coyotesqrl.2024.day03
  (:require
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
^{::clerk/visibility {:result :hide}}
(def day3-input
  (->> "coyotesqrl/2024/day3-input.txt"
       (utils/input->str)))

^{::clerk/visibility {:result :hide}}
(defn parse-input [input]
  (let [matcher (re-matcher #"(.*?(mul\(\d{1,3},\d{1,3}\)).*?)+?" input)]
    (loop [a (list)
           mul (last (re-find matcher))]
      (if mul
        (recur (conj a mul) (last (re-find matcher)))
        a))))

^{::clerk/visibility {:result :hide}}
(defn perform-mult [input]
  (->> input
       (map #((juxt second last) (re-find #"(\d{1,3}),(\d{1,3})" %)))
       (map #(map parse-long %))
       (map (fn [[l r]] (* l r)))
       (reduce +)))

;; Result
(->> day3-input
     (parse-input)
     (perform-mult)
     (utils/answer-block))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn parse-input-2 [input]
  (let [matcher (re-matcher #"(.*?(mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)).*?)+?" input)]
    (loop [a (list)
           do? true
           term (last (re-find matcher))]
      (if term
        (let [next-term (last (re-find matcher))]
          (cond
            (= term "do()") (recur a true next-term)
            (= term "don't()") (recur a false next-term)
            do? (recur (conj a term) true next-term)
            :else (recur a do? next-term)))
        a))))

(->> day3-input
     (parse-input-2)
     (perform-mult)
     (utils/answer-block))
