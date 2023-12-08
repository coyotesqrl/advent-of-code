;; ## Advent of Code 2023
;; ### [Day 8](https://adventofcode.com/2023/day/8)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day08.clj)

(ns coyotesqrl.2023.day08
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [clojure.math.numeric-tower :as math]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (->> "coyotesqrl/2023/day8-input.txt"
       (utils/input->seq)))

^{::clerk/visibility {:result :hide}}
(defn get-instructions [input]
  (seq (first input)))

^{::clerk/visibility {:result :hide}}
(defn parse-line [[node lr]]
  (let [[_ l r] (re-find #"(\w+), (\w+)" lr)]
    [(str/trim node) [l r]]))

^{::clerk/visibility {:result :hide}}
(defn get-network [input]
  (->> input
       (drop 2)
       (map #(str/split % #"="))
       (map (juxt first last))
       (map parse-line)
       (into {})))

;; ##### Process steps
^{::clerk/visibility {:result :hide}}
(def max-allowed-path-length 100000)

^{::clerk/visibility {:result :hide}}
(defn step-to-z [instructions network start-node end-node]
  (reduce (fn [a v]
            (let [node      (get network (:loc a))
                  next-step (nth node (if (= v \L) 0 1))
                  cnt       (inc (:cnt a))]
              (if (> cnt max-allowed-path-length)
                (reduced -1)
                (if (= end-node next-step)
                  (reduced cnt)
                  {:cnt cnt :loc next-step}))))
          {:cnt 0 :loc start-node}
          (cycle (seq instructions))))

;; ##### Solve
(utils/answer-block (step-to-z (get-instructions part1-input)
                               (get-network part1-input)
                               "AAA"
                               "ZZZ"))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn get-min-paths []
  (let [nodes       (keys (get-network part1-input))
        start-nodes (filter #(= \A (last %)) nodes)
        end-nodes   (filter #(= \Z (last %)) nodes)
        inst        (get-instructions part1-input)
        network     (get-network part1-input)]
    (for [s start-nodes
          e end-nodes]
      [s e (step-to-z inst network s e)])))

^{::clerk/visibility {:result :hide}}
(defn least-common-multiple [paths]
  (reduce (fn [a v]
            (math/lcm a v))
          (first paths)
          (rest paths)))

;; ##### Solve
(->> (get-min-paths)
     (filter #(pos-int? (last %)))
     (map last)
     (least-common-multiple)
     (utils/answer-block))
