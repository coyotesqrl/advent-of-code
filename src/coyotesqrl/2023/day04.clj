;; ## Advent of Code 2023
;; ### [Day 4](https://adventofcode.com/2023/day/4)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day04.clj)

(ns coyotesqrl.2023.day04
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (->> "coyotesqrl/2023/day4-input.txt"
       (utils/input->seq)))

;; ##### Process card
^{::clerk/visibility {:result :hide}}
(defn parse-numbers [s]
  (->> (str/split s #" ")
       (map parse-long)
       (filter some?)
       (set)))

^{::clerk/visibility {:result :hide}}
(defn match-count [winners mine]
  (count (keep winners mine)))

^{::clerk/visibility {:result :hide}}
(defn card-value [matches]
  (if (pos-int? matches)
    (math/expt 2 (dec matches))
    0))

^{::clerk/visibility {:result :hide}}
(defn parse-card [c]
  (let [[card numbers] (str/split c #":")
        card-no (parse-long (re-find #"\d+" card))
        [winners mine] (str/split numbers #"\|")
        winners (parse-numbers winners)
        mine (parse-numbers mine)
        matches (match-count winners mine)]
    {:card card :card-no card-no :winners winners :mine mine :matches matches :value (card-value matches)}))

;; ##### Solve
(->> part1-input
     (map parse-card)
     (map :value)
     (apply +)
     (utils/answer-block))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn make-card-map [input]
  (update-vals (->> input
                    (map parse-card)
                    (group-by :card-no)
                    (map (fn [r] (update r 1 #(cons 1 %))))
                    (into {}))
               vec))

^{::clerk/visibility {:result :hide}}
(defn get-mo-cards [acc row-num]
  (let [matches   (get-in acc [row-num 1 :matches])
        last-card (apply max (keys acc))
        next-card (inc row-num)
        inc-by    (get-in acc [row-num 0])]
    (reduce (fn [a v]
              (if (> v last-card)
                a
                (update-in a [v 0] #(+ inc-by %))))
            acc
            (range next-card (+ next-card matches)))))

^{::clerk/visibility {:result :hide}}
(defn process-cards [input]
  (let [card-map  (make-card-map input)
        last-card (apply max (keys card-map))]
    (loop [acc     card-map
           row-num 1]
      (if (= row-num last-card)
        acc
        (recur (get-mo-cards acc row-num) (inc row-num))))))

;; ##### Solve
(->> part1-input
     (process-cards)
     (vals)
     (map #(get % 0))
     (apply +)
     (utils/answer-block))
