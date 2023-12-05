;; ## Advent of Code 2023
;; ### [Day 5](https://adventofcode.com/2023/day/5)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day05.clj)

(ns coyotesqrl.2023.day05
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (-> "coyotesqrl/2023/day5-input.txt"
      (utils/input->str)
      (str/split #"\n\n")))

(def sample
  (-> "coyotesqrl/2023/day5-sample.txt"
      (utils/input->str)
      (str/split #"\n\n")))

;; ##### Process
^{::clerk/visibility {:result :hide}}
(defn extract-numbers [s]
  (->> (str/split s #"\s")
       (remove str/blank?)
       (map parse-long)))

^{::clerk/visibility {:result :hide}}
(defn get-seeds [input]
  (-> input
      (first)
      (str/split #":")
      (second)
      (extract-numbers)))

^{::clerk/visibility {:result :hide}}
(defn parse-map [m]
  (let [input (str/split m #"\n")
        stage (-> input (first) (str/split #" ") (first) (keyword))
        mappings (->> input
                      (rest)
                      (map extract-numbers))]
    {stage mappings}))

^{::clerk/visibility {:result :hide}}
(defn get-all-maps [input]
  (->> (rest input)
       (map parse-map)
       (into {})))


^{::clerk/visibility {:result :hide}}
(defn lookup [source m]
  (reduce (fn [a [dest-start source-start len]]
            (if (<= source-start source (dec (+ source-start len)))
              (reduced (+ dest-start (- source source-start)))
              a))
          source
          m))

^{::clerk/visibility {:result :hide}}
(defn get-location [seed maps]
  (-> seed
      (lookup (:seed-to-soil maps))
      (lookup (:soil-to-fertilizer maps))
      (lookup (:fertilizer-to-water maps))
      (lookup (:water-to-light maps))
      (lookup (:light-to-temperature maps))
      (lookup (:temperature-to-humidity maps))
      (lookup (:humidity-to-location maps))))

;; ##### Solve
(let [seeds (get-seeds part1-input)
      maps  (get-all-maps part1-input)]
  (->> seeds
       (map #(get-location % maps))
       (apply min)
       (utils/answer-block)))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn get-seed-ranges [input]
  (let [numbers (-> input
                    (first)
                    (str/split #":")
                    (second)
                    (extract-numbers))]
    (partition 2 numbers)))

^{::clerk/visibility {:result :hide}}
(defn get-range-location [[seed-start len] maps]
  (reduce (fn [a v]
            (min a (get-location v maps)))
          (Long/MAX_VALUE)
          (range seed-start (+ seed-start len))))

;; ##### Solve
(let [input part1-input
      seed-ranges (get-seed-ranges input)
      maps  (get-all-maps input)]
  (->> seed-ranges
       (map #(get-range-location % maps))
       (apply min)
       (utils/answer-block)))

(comment
  (let [seed-ranges (get-seed-ranges sample)
        maps  (get-all-maps sample)]
    (->> seed-ranges
         (map #(get-range-location % maps))
         (apply min)
         #_(utils/answer-block)))

  (get-seed-ranges sample)

  (for [i (range 0 101)]
    [i (lookup i (:seed-to-soil (get-all-maps sample)))])

  (map #(get-location % (get-all-maps sample)) (get-seeds sample))
  (apply min (map #(get-location % (get-all-maps part1-input)) (get-seeds part1-input)))

  (tap> (get-all-maps sample))
  (tap> (get-all-maps part1-input)))

;; ---
