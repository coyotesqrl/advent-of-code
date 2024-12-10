;; ## Advent of Code 2024
;; ### [Day 4](https://adventofcode.com/2024/day/4)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2024/day04.clj)

(ns coyotesqrl.2024.day04
  (:require
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
^{::clerk/visibility {:result :hide}}
(def example-input
  '("MMMSXXMASM"
    "MSAMXMSMSA"
    "AMXSXMAAMM"
    "MSAMASMSMX"
    "XMASAMXAMM"
    "XXAMMXXAMA"
    "SMSMSASXSS"
    "SAXAMASAAA"
    "MAMMMXMMMM"
    "MXMXAXMASX"))

^{::clerk/visibility {:result :hide}}
(def day4-input
  (->> "coyotesqrl/2024/day4-input.txt"
       (utils/input->seq)))

^{::clerk/visibility {:result :hide}}
(defn map-letters [input]
  (->> input
       (map-indexed vector)
       (map (fn [[k v]] (vector k (map-indexed vector v))))
       (mapcat (fn [[k v]] (map (fn [[i l]] {[k i] l}) v)))
       (filter (fn [v] (#{\X \M \A \S} (first (vals v)))))
       (into {})))

^{::clerk/visibility {:result :hide}}
(defn build-path [letter dx dy len]
  (let [[[i j] _] letter]
    (for [t (range 1 (+ 1 len))]
      [(+ i (* t dx)) (+ j (* t dy))])))

^{::clerk/visibility {:result :hide}}
(defn build-paths [letter len]
  (for [x (range -1 2)
        y (range -1 2)]
    (build-path letter x y len)))

^{::clerk/visibility {:result :hide}}
(defn path-search [input path letter]
  (let [paths (build-paths letter (count path))]
    (reduce (fn [a v]
              (let [p (map #(get input %) v)]
                (if (= path p)
                  (conj a (cons letter v))
                  a)))
            '()
            paths)))

^{::clerk/visibility {:result :hide}}
(defn word-search [word input]
  (let [letter (first word)
        path   (rest word)]
    (->> input
         (filter (fn [[_ v]] (= letter v)))
         (mapcat (partial path-search input path)))))

;; Sample result
(->> example-input
     (map-letters)
     (word-search "XMAS")
     (filter not-empty)
     (count))

;; Result
(->> day4-input
     (map-letters)
     (word-search "XMAS")
     (filter not-empty)
     (count)
     (utils/answer-block))

;; ---
;; #### Part 2
^{::clerk/visibility {:result :hide}}
(defn find-x [input letter]
  (let [[[i j] _] letter
        match-set #{\M \S}
        x1 (set (list (get input [(dec i) (dec j)])
                      (get input [(inc i) (inc j)])))
        x2 (set (list (get input [(inc i) (dec j)])
                      (get input [(dec i) (inc j)])))]
    (and (= match-set x1) (= match-set x2))))

^{::clerk/visibility {:result :hide}}
(defn x-mas-search [input]
  (->> input
       (filter (fn [[_ v]] (= \A v)))
       (map (partial find-x input))
       (filter true?)))

; Sample result
(->> example-input
     (map-letters)
     (x-mas-search)
     (count))

;; Result
(->> day4-input
     (map-letters)
     (x-mas-search)
     (count)
     (utils/answer-block))
