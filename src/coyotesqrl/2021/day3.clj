;; ## Advent of Code 2021
;; ### Day 3
(ns coyotesqrl.2021.day3
  (:require [coyotesqrl.utils :as utils]))

;; Input
(def day3-input (utils/input->seq "coyotesqrl/2021/day3-input.txt"))

;; #### Part 1
;; The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic
;; report just in case.
;;
;; The diagnostic report (your puzzle input) consists of a list of binary numbers which, when
;; decoded properly, can tell you many useful things about the conditions of the submarine. The
;; first parameter to check is the power consumption.
;;
;; You need to use the binary numbers in the diagnostic report to generate two new binary numbers
;; (called the gamma rate and the epsilon rate). The power consumption can then be found by
;; multiplying the gamma rate by the epsilon rate.
;;
;; Each bit in the gamma rate can be determined by finding the most common bit in the corresponding
;; position of all numbers in the diagnostic report.
;;
;; The epsilon rate is calculated in a similar way; rather than use the most common bit, the least
;; common bit from each position is used.
;;
;; Multiplying the gamma rate by the epsilon rate produces the power consumption.

(defn max-val
  ([r] (max-val r \1))
  ([r default]
   (let [z (get r \0 0)
         o (get r \1 0)]
     (cond
       (> z o) \0
       (> o z) \1
       :else default))))

(defn most-common-columns
  "Breaks input strings into vectors of one-character strings,
  rotates matrix,
  counts 0s and 1s,
  and finally returns a vector of the most common value in each column."
  [in]
  (->> in
       (map #(apply vector %))
       (apply map vector)
       (map frequencies)
       (map max-val)))

(defn invert [n]
  (let [mask-str (apply str (take (Integer/bitCount n) (repeat "1")))
        mask (Long/parseLong mask-str 2)]
    (bit-and mask (bit-not n))))

(defn epsilon [in]
  (Long/parseLong (apply str (most-common-columns in)) 2))

(let [eps (epsilon day3-input)
      gam (invert eps)]
  (* eps gam))

;; ---
;; #### Part 2
;; Next, you should verify the life support rating, which can be determined by multiplying the
;; oxygen generator rating by the CO2 scrubber rating.
;;
;; Both values are located using a similar process that involves filtering out values until only
;; one remains. Before searching for either rating value, start with the full list of binary numbers
;; from your diagnostic report and consider just the first bit of those numbers. Then:
;;
;; * Keep only numbers selected by the bit criteria for the type of rating value for which you are
;;   searching. Discard numbers which do not match the bit criteria.
;; * If you only have one number left, stop; this is the rating value for which you are searching.
;; * Otherwise, repeat the process, considering the next bit to the right.
;;
;; The bit criteria depends on which type of rating value you want to find:
;;
;; * To find oxygen generator rating, determine the most common value (0 or 1) in the current bit
;;   position, and keep only numbers with that bit in that position. If 0 and 1 are equally common,
;;   keep values with a 1 in the position being considered.
;; * To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit position,
;;   and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values
;;   with a 0 in the position being considered.

(defn max-nth [in n]
  (nth (most-common-columns in) n))

(defn air-raters [f in n]
  (let [max-n (max-nth in n)]
    (f #(= max-n (nth % n)) in)))

(defn system->rating [find-func]
  (loop [in day3-input
         n 0]
    (if (= (count in) 1)
      (first in)
      (recur (find-func in n) (inc n)))))

(let [o2-rating (system->rating (partial air-raters filter))
      co2-rating (system->rating (partial air-raters remove))]
  (* (Long/parseLong o2-rating 2) (Long/parseLong co2-rating 2)))
