;; ## Advent of Code 2021
;; ### Day 4
(ns coyotesqrl.2021.day04
  (:require [coyotesqrl.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

;; Input
(def day4-input (utils/input->seq "coyotesqrl/2021/day4-input.txt"))

;; #### Part 1
;; Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen
;; at random, and the chosen number is marked on all boards on which it appears. (Numbers may not
;; appear on all boards.) If all numbers in any row or any column of a board are marked, that board
;; wins. (Diagonals don't count.)
;;
;; Input is in the form:
;; ```
;; 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
;;
;; 22 13 17 11  0
;;  8  2 23  4 24
;; 21  9 14 16  7
;;  6 10  3 18  5
;;  1 12 20 15 19
;;
;;  3 15  0  2 22
;;  9 18 13 17  5
;; 19  8  7 25 23
;; 20 11 10 24  4
;; 14 21 16 12  6
;; ```
;; where the first row is the numbers to be called out and the remaining rows represent the 5x5 grids.
;;
;; The score for the winning board is calculated as the **sum of unmarked numbers on the board**
;; multiplied by the **winning number** called.
(defn called-numbers [in]
  (->> (str/split (first in) #",")
       (map #(Long/parseLong %))))

(defn boards [in]
  (->> in
       rest
       (remove str/blank?)
       (map str/trim)
       (map #(str/split % #"\s+"))
       (map (fn [l] (map #(Integer/parseInt %) l)))
       (partition 5)
       (map flatten)
       (map #(map-indexed (fn [k v] {v k}) %))))

(defn process-number
  "Process one board with a called number."
  [b n]
  (if-let [matches (->> b
                        (filter #(contains? % n))
                        (map #(get % n)))]
    (with-meta b {:matches (into matches (:matches (meta b)))})
    b))

(defn check-board
  "Checks each row and column of the board to find if one is filled, by looking at the metadata
  carried along with each board."
  [b]
  (let [m (set (:matches (meta b)))
        rows (for [r (range 5 26 5)]
               (set/select #(< (- r 6) % r) m))
        cols (for [c (range 0 5)]
               (set/select #(= c (mod % 5)) m))]
    (> (->> (into rows cols)
            (filter #(= 5 (count %)))
            count)
       0)))

(defn sum-unmatched [b]
  (let [found (:matches (meta b))
        red #(reduce-kv (fn [_ k v] [v k]) nil %)]
    (->> b
         (map red)
         (remove (fn [[k _]] (contains? (set found) k)))
         (map second)
         (apply +))))

(defn boards->nth-winner
  [boards to-call n]
  (loop [[num & to-call] to-call
         boards boards
         fnd-cnt -1]
    (let [{done true not-done false} (->> boards
                                          (map #(process-number % num))
                                          (group-by check-board))]
      (if (>= (+ fnd-cnt (count done)) n)
        (* num (sum-unmatched (nth done (- n fnd-cnt 1))))
        (recur to-call
               not-done
               (+ fnd-cnt (count done)))))))

(boards->nth-winner (boards day4-input)
                    (called-numbers day4-input)
                    0)

;; ---
;; #### Part 2
;; On the other hand, it might be wise to try a different strategy: let the giant squid win.
;;
;; You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time
;; counting its arms, the safe thing to do is to figure out which board will win last and choose
;; that one. That way, no matter which boards it picks, it will win for sure.
;;
;; Figure out which board will win last. Once it wins, what would its final score be?
(boards->nth-winner (boards day4-input)
                    (called-numbers day4-input)
                    99)
