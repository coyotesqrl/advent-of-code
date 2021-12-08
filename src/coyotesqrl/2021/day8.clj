;; ## Advent of Code 2021
;; ### Day 8
(ns coyotesqrl.2021.day8
  (:require [coyotesqrl.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

;; ##### Input
(def day8-input (->> (utils/input->seq "coyotesqrl/2021/day8-input.txt")
                     (map #(str/split % #" [|] "))
                     (map (fn [[s o]] [(str/split s #" ") (str/split o #" ")]))))

;; #### Part 1
;; Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit
;; output value. Within an entry, the same wire/segment connections are used (but you don't know
;; what the connections actually are). The unique signal patterns correspond to the ten different
;; ways the submarine tries to render a digit using the current wire/segment connections.
;; Because 7 is the only digit that uses three segments, dab in the above example means that to
;; render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments,
;; eafb means that to render a 4, signal lines e, a, f, and b are on.
;;
;; Using this information, you should be able to work out which combination of signal wires
;; corresponds to each of the ten digits. Then, you can decode the four digit output value.
;; Unfortunately, in the above example, all of the digits in the output value
;; (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.
;;
;; Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to
;; tell which combinations of signals correspond to those digits. Counting only digits in the output
;; values (the part after | on each line), in the above example, there are 26 instances of digits
;; that use a unique number of segments (highlighted above).
;;
;; **In the output values, how many times do digits 1, 4, 7, or 8 appear?**

(time (->> day8-input
           (map second)
           flatten
           (filter #(#{2 3 7 4} (count %)))
           count))

;; ---
;; #### Part 2
;; Through a little deduction, you should now be able to determine the remaining digits.
;;
;; For each entry, determine all of the wire/segment connections and decode the four-digit output
;; values. **What do you get if you add up all of the output values?**

;; ###### Mapping rules:
;; 1. The single two-segment number is 1
;; 2. The single three-segment number is 7
;; 3. The single four-segment number is 4
;; 4. The single seven-segment number is 8
;; 5. The six-segment number with **only one** intersection with 1 is 6
;; 6. The six-segment number with **four** intersections with 4 is 9
;; 7. The remaining six-segment number is 0
;; 8. The five-segment number with **two** intersections with 4 is 2
;; 9. The five-segment number with **two** intersections with 1 is 3
;; 10. The remaining five-segment number is 5

(defn- get-key-by-val [m k]
  (get (set/map-invert m) k))

(defn- intersect-count [s1 s2]
  (count (set/intersection (set s1) (set s2))))

;; Rules 1-4
(defn- init-lookup
  "Initialize a lookup map with the four simplest mappings."
  [signals]
  (reduce (fn [a v]
            (assoc a (set v) (case (count v)
                               2 "1"
                               3 "7"
                               4 "4"
                               7 "8"
                               nil)))
          {}
          signals))

(defn- get-by-intersects [signal-map seg-cnt i-num i-val val]
  (assoc signal-map
         (->> signal-map
              keys
              (filter #(= seg-cnt (count %)))
              (filter #(= i-num (intersect-count % (get-key-by-val signal-map i-val))))
              first
              set)
         val))

(defn- get-last-by-seg-cnt [signal-map seg-cnt val]
  (assoc signal-map
         (->> signal-map
              (filter (fn [[k _]] (= seg-cnt (count k))))
              (filter (fn [[_ v]] (nil? v)))
              ffirst
              set)
         val))

;; Rule 5: The six-segment number with **only one** intersection with 1 is 6
(defn- get-six [signal-map]
  (get-by-intersects signal-map 6 1 "1" "6"))

;; Rule 6: The six-segment number with **four** intersections with 4 is 9
(defn- get-nine [signal-map]
  (get-by-intersects signal-map 6 4 "4" "9"))

;; Rule 7: The remaining six-segment number is 0
(defn- get-zero [signal-map]
  (get-last-by-seg-cnt signal-map 6 "0"))

;; Rule 8: The five-segment number with **two** intersections with 4 is 2
(defn- get-two [signal-map]
  (get-by-intersects signal-map 5 2 "4" "2"))

;; Rule 9: The five-segment number with **two** intersections with 1 is 3
(defn- get-three [signal-map]
  (get-by-intersects signal-map 5 2 "1" "3"))

;; Rule 10: The remaining five-segment number is 5
(defn- get-five [signal-map]
  (get-last-by-seg-cnt signal-map 5 "5"))

(defn- signals->map [signals]
  (->> signals
       init-lookup
       get-six
       get-nine
       get-zero
       get-two
       get-three
       get-five))

;; > _N.B. Digits were mapped as strings instead of numbers to simplify final processing. It would
;;   not be too hard, presumably, to add thusly: (+ (* 1000 first) (* 100 second) etc.); however
;;   it was hard enough for me to contemplate this evening._
(defn map-numbers [[signals output]]
  (let [signal-map (signals->map signals)]
    (->> output
         (map #(get signal-map (set %)))
         str/join
         Long/parseLong)))

(time (->> day8-input
           (map map-numbers)
           (apply +)))
