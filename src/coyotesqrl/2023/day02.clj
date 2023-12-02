;; ## Advent of Code 2023
;; ### [Day 2](https://adventofcode.com/2023/day/2)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day02.clj)

(ns coyotesqrl.2023.day02
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1

;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (->> "coyotesqrl/2023/day2-input.txt"
       (utils/input->seq)))

;; ##### Processing/parsing functions
^{::clerk/visibility {:result :hide}}
(defn game-pulls [g]
  (map str/trim (-> g
                    (str/replace #"," "")
                    (str/split #":")
                    (second)
                    (str/trim)
                    (str/split #";"))))

^{::clerk/visibility {:result :hide}}
(defn pull-counts [v]
  (reduce (fn [a v]
            (assoc a (second v) (parse-long (first v))))
          {}
          v))

^{::clerk/visibility {:result :hide}}
(defn max-count [m k]
  (let [counts (->> m
                    (map #(get % k))
                    (remove nil?))]
    (when (seq counts)
      (apply max counts))))

^{::clerk/visibility {:result :hide}}
(defn game-maxes [g]
  (let [game-num    (->> g
                         (re-find #"Game (\d*):")
                         (second)
                         (parse-long))
        pulls       (game-pulls g)
        pull-counts (->> pulls
                         (map #(str/split % #" "))
                         (map #(partition 2 %))
                         (map vec)
                         (map pull-counts))]
    {:game  game-num
     :red   (max-count pull-counts "red")
     :green (max-count pull-counts "green")
     :blue  (max-count pull-counts "blue")}))

^{::clerk/visibility {:result :hide}}
(defn game-possible? [red green blue game]
  (and (>= red (:red game))
       (>= green (:green game))
       (>= blue (:blue game))))

;; ##### Part 1 result
(->> part1-input
     (map game-maxes)
     (filter (partial game-possible? 12 13 14))
     (map :game)
     (apply +)
     (utils/answer-block))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn game-power [g]
  (* (:red g) (:green g) (:blue g)))

(->> part1-input
     (map game-maxes)
     (map game-power)
     (apply +)
     (utils/answer-block))
