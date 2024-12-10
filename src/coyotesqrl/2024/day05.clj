;; ## Advent of Code 2024
;; ### [Day 5](https://adventofcode.com/2024/day/5)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2024/day05.clj)

(ns coyotesqrl.2024.day05
  (:require
   [clojure.math.combinatorics :as combo]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
^{::clerk/visibility {:result :hide}}
(def example-input
  '("47|53"
    "97|13"
    "97|61"
    "97|47"
    "75|29"
    "61|13"
    "75|53"
    "29|13"
    "97|29"
    "53|29"
    "61|53"
    "97|53"
    "61|29"
    "47|13"
    "75|47"
    "97|75"
    "47|61"
    "75|61"
    "47|29"
    "75|13"
    "53|13"

    "75,47,61,53,29"
    "97,61,53,29,13"
    "75,29,13"
    "75,97,47,61,53"
    "61,13,29"
    "97,13,75,29,47"))

^{::clerk/visibility {:result :hide}}
(def day5-input
  (->> "coyotesqrl/2024/day5-input.txt"
       (utils/input->seq)))

^{::clerk/visibility {:result :hide}}
(defn parser [input]
  (reduce (fn [a v]
            (condp re-matches v
              #"(\d+)\|(\d+)" :>> (fn [[_x l r]] (update a :rules #(conj % [(parse-long l) (parse-long r)])))
              #"\d+,\d.*" :>> (fn [_] (update a :runs #(conj % (map parse-long (re-seq #"\d+" v)))))
              a))
          {:rules [] :runs []}
          input))

^{::clerk/visibility {:result :hide}}
(defn check-order [rules run]
  (let [pairs (combo/combinations run 2)
        applicable-rules (->> rules
                              (filter #((set (map set pairs)) (set %)))
                              (set))]
    (reduce (fn [_a v]
              (cond
                (applicable-rules v) true
                (applicable-rules (vec (reverse v))) (reduced false)
                :else true))
            true
            pairs)))

;; Sample result
(let [{:keys [rules runs]} (parser example-input)]
  (->> runs
       (filter (partial check-order rules))
       (map #(nth % (quot (count %) 2)))
       (reduce +)))

;; Result
(let [{:keys [rules runs]} (parser day5-input)]
  (->> runs
       (filter (partial check-order rules))
       (map #(nth % (quot (count %) 2)))
       (reduce +)
       (utils/answer-block)))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn re-sort-run [rules run]
  (let [c (comparator (fn [x y]
                        (let [[l r] (->> rules
                                         (filter #(= (set %) #{x y}))
                                         (first))]
                          (if (< l r)
                            (< x y)
                            (> x y)))))]
    (sort c run)))

;; Sample result
(let [{:keys [rules runs]} (parser example-input)]
  (->> runs
       (filter #(not (check-order rules %)))
       (map (partial re-sort-run rules))
       (map #(nth % (quot (count %) 2)))
       (reduce +)))

;; Result
(let [{:keys [rules runs]} (parser day5-input)]
  (->> runs
       (filter #(not (check-order rules %)))
       (map (partial re-sort-run rules))
       (map #(nth % (quot (count %) 2)))
       (reduce +)
       (utils/answer-block)))
