;; ## Advent of Code 2023
;; This means nothing
;; ### [Day 1](https://adventofcode.com/2023/day/1)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day01.clj)

(ns coyotesqrl.2023.day01
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (->> "coyotesqrl/2023/day1-input.txt"
       (utils/input->seq)))

;; For the first part, we just need to get the first/last digits in the input strings to generate two-digit numbers
;; to be summed. In the case where there is only one digit, it is both first and last.
(let [digits (set "123456789")]
  (->> part1-input
       (map #(keep digits %))
       (map (juxt first last))
       (map #(apply str %))
       (map parse-long)
       (apply +)
       (utils/answer-block)))

;; ---
;; #### Part 2
;; In this part, we'll need to treat spelled-out numbers as numbers.
^{::clerk/visibility {:result :hide}}
(defn numeralize [s]
  (condp = s
    "one" "1"
    "two" "2"
    "three" "3"
    "four" "4"
    "five" "5"
    "six" "6"
    "seven" "7"
    "eight" "8"
    "nine" "9"
    s))

^{::clerk/visibility {:result :hide}}
(defn get-number [indexes pos-fn order-fn]
  (let [x (->> indexes (map pos-fn) (into {}))
        k (apply order-fn (remove nil? (keys x)))]
    (numeralize (get x k))))

^{::clerk/visibility {:result :hide}}
(defn get-two-digit-string [s]
  (let [numbers '("1" "2" "3" "4" "5" "6" "7" "8" "9"
                      "one" "two" "three" "four" "five" "six"
                      "seven" "eight" "nine")
        indexes (for [n numbers]
                  [{(str/index-of s n) n} {(str/last-index-of s n) n}])]
    (str (get-number indexes first min) (get-number indexes second max))))

(->> part1-input
     (map get-two-digit-string)
     (map parse-long)
     (apply +)
     (utils/answer-block))
;; ---
