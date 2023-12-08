;; ## Advent of Code 2023
;; ### [Day 7](https://adventofcode.com/2023/day/7)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day07.clj)

(ns coyotesqrl.2023.day07
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (update-vals (->> "coyotesqrl/2023/day7-input.txt"
                    (utils/input->seq)
                    (map #(str/split % #"\s"))
                    (into {}))
               parse-long))

;; ##### Process hands
^{::clerk/visibility {:result :hide}}
(defn raw-hand-ranker [hand]
  (let [groups (-> (group-by identity hand)
                   (update-vals count))
        pair-checker (fn [grp-cnt max-match groups]
                       (and (= grp-cnt (count groups))
                            (= max-match (apply max (vals groups)))))]
    (cond
      (= 5 (count (distinct hand))) 1     ;; no matching cards; high card
      (= 4 (count (distinct hand))) 2     ;; one pair
      (pair-checker 3 2 groups) 3         ;; two pair
      (pair-checker 3 3 groups) 4         ;; three of a kind
      (pair-checker 2 3 groups) 5         ;; full house
      (pair-checker 2 4 groups) 6         ;; four of a kind
      (= 1 (count (distinct hand))) 7)))  ;; five of a kind

^{::clerk/visibility {:result :hide}}
(def ranked-cards (set/map-invert (into {} (map-indexed vector (seq "23456789TJQKA")))))

^{::clerk/visibility {:result :hide}}
(defn hand-comparator [hand-ranker card-ranks]
  (comparator (fn [l r]
                (let [left-rank  (hand-ranker l)
                      right-rank (hand-ranker r)]
                  (if (= left-rank right-rank)
                    (reduce (fn [a [lv rv]]
                              (if (= lv rv)
                                a
                                (reduced (< (get card-ranks lv) (get card-ranks rv)))))
                            0
                            (partition 2 (interleave l r)))
                    (< left-rank right-rank))))))

^{::clerk/visibility {:result :hide}}
(defn order-hands [hand-ranker card-ranker hands]
  (sort (hand-comparator hand-ranker card-ranker) hands))

^{::clerk/visibility {:result :hide}}
(defn calculate-winnings [input hand-ranker card-ranker]
  (let [ordered-hands (order-hands hand-ranker card-ranker (keys input))]
    (reduce (fn [a [rank hand]]
              (+ a (* rank (get input hand))))
            0
            (update-keys (->> ordered-hands
                              (map-indexed vector)
                              (into {}))
                         inc))))

;; ##### Solve
(utils/answer-block (calculate-winnings part1-input raw-hand-ranker ranked-cards))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(def joker-ranked-cards (set/map-invert (into {} (map-indexed vector (seq "J23456789TQKA")))))

^{::clerk/visibility {:result :hide}}
(defn joker-hand-ranker [hand]
  (if (re-find #"J" hand)
    (reduce (fn [a v]
              (max a (raw-hand-ranker (str/replace hand #"J" (str v)))))
            0
            (seq "23456789TQKA"))
    (raw-hand-ranker hand)))

;; ##### Solve
(utils/answer-block (calculate-winnings part1-input joker-hand-ranker joker-ranked-cards))
