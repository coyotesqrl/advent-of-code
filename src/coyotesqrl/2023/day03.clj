;; ## Advent of Code 2023
;; ### [Day 3](https://adventofcode.com/2023/day/3)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2023/day03.clj)

(ns coyotesqrl.2023.day03
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; ##### Load input data
^{::clerk/visibility {:result :hide}}
(def part1-input
  (->> "coyotesqrl/2023/day3-input.txt"
       (utils/input->seq)))

;; ##### Processing/parsing functions
^{::clerk/visibility {:result :hide}}
(defn get-symbol-row [row]
  (->> row
       (map-indexed vector)
       (remove #(= \. (second %)))
       (remove #((set "0123456789") (second %)))
       (into {})))

^{::clerk/visibility {:result :hide}}
(defn get-symbol-map [input]
  (->> input
       (map #(get-symbol-row %))
       (vec)))

^{::clerk/visibility {:result :hide}}
(defn get-numbers-from-row [row]
  (->> row
       (partition-by #(contains? (set "0123456789") %))
       (map #(apply str %))
       (map parse-long)
       (filter number?)))

^{::clerk/visibility {:result :hide}}
(defn get-indexed-numbers-from-row [row]
  (let [ns (get-numbers-from-row row)]
    (if (seq ns)
      (:vals (reduce (fn [a v]
                       (let [idx (str/index-of row (str v) (:idx a))
                             new-idx (+ idx (count (str v)))]
                         (-> a
                             (assoc :idx new-idx)
                             (update :vals conj [v idx new-idx]))))
                     {:idx 0 :vals []}
                     ns))
      '())))

^{::clerk/visibility {:result :hide}}
(defn get-indexed-numbers [sample]
  (vec (map get-indexed-numbers-from-row sample)))

^{::clerk/visibility {:result :hide}}
(defn number-abuts-symbol? [n row-idx symbol-map]
  (let [left          (dec (second n))
        right         (+ (second n) (count (str (first n))))
        found-symbols (for [r (range (dec row-idx) (+ 2 row-idx))]
                        (for [c (range left (inc right))]
                          (get-in symbol-map [r c])))]
    (->> found-symbols
         (flatten)
         (remove nil?)
         (not-empty))))

^{::clerk/visibility {:result :hide}}
(defn value-of-number
  "returns part number or zero, depending on whether it abuts a symbol"
  [n row-idx symbol-map]
  (if (number-abuts-symbol? n row-idx symbol-map)
    (first n)
    0))

^{::clerk/visibility {:result :hide}}
(defn sum-parts [input]
  (let [symbol-map (get-symbol-map input)]
    (reduce-kv
     (fn [a k v]
       (if (seq v)
         (+ a (->> v
                   (map #(value-of-number % k symbol-map))
                   (apply +)))
         a))
     0
     (get-indexed-numbers input))))

;; ##### Part 1 result
(->> part1-input
     (sum-parts)
     (utils/answer-block))

;; ---
;; #### Part 2

;; ##### Processing/parsing functions
^{::clerk/visibility {:result :hide}}
(defn get-gear-row [row]
  (->> row
       (map-indexed vector)
       (filter #(= \* (second %)))
       (into {})))

^{::clerk/visibility {:result :hide}}
(defn get-gear-map [input]
  (let [gear-rows (->> input
                       (map #(get-gear-row %))
                       (map-indexed vector)
                       (filter #(not-empty (second %))))]
    (reduce (fn [a v]
              (into a (for [g (second v)]
                        [(first v) (first g)])))
            []
            gear-rows)))

^{::clerk/visibility {:result :hide}}
(defn adjacent-to-gear? [gear-idx n]
  (<= (dec (second n)) gear-idx (last n)))

^{::clerk/visibility {:result :hide}}
(defn gear-ratio [gear indexed-numbers]
  (let [row-idx       (first gear)
        nums-to-check (-> []
                          (into (get indexed-numbers row-idx))
                          (into (get indexed-numbers (dec row-idx)))
                          (into (get indexed-numbers (inc row-idx))))
        gear-elements (filter #(adjacent-to-gear? (second gear) %) nums-to-check)]
    (if (= 2 (count gear-elements))
      (->> gear-elements
           (map first)
           (apply *))
      0)))

(let [gear-map (get-gear-map part1-input)
      indexed-numbers (get-indexed-numbers part1-input)]
  (->> gear-map
       (map #(gear-ratio % indexed-numbers))
       (apply +)
       (utils/answer-block)))
