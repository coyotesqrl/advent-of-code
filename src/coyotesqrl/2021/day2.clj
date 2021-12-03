;; ## Advent of Code 2021
(ns coyotesqrl.2021.day2
  (:require [coyotesqrl.utils :as utils]
            [clojure.string :as str]))

;; ### Day 2
;; Input as direction & distance
(def day2-input (->> (utils/input->seq "coyotesqrl/2021/day2-input.txt")
                     (map #(str/split % #"\s"))
                     (map (fn [r] (update r 1 #(Long/parseLong %))))))

;; #### Part 1
;; Now, you need to figure out how to pilot this thing.

;; It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:

;; * forward X *increases* the horizontal position by X units.
;; * down X *increases* the depth by X units.
;; * up X *decreases* the depth by X units.

;; Calculate the horizontal position and depth you would have after following the planned course.
;; What do you get if you multiply your final horizontal position by your final depth?
(defn move-sub-naive [input]
  (let [pos [0 0]]
    (->> (reduce (fn [a v]
                   (let [[dir amt] v]
                     (case dir
                       "forward" (update a 0 + amt)
                       "down" (update a 1 + amt)
                       "up" (update a 1 - amt))))
                 pos
                 input)
         (apply *))))

(move-sub-naive day2-input)

;; ---
;; #### Part 2
;; In addition to horizontal position and depth, you'll also need to track a third value, aim, which
;; also starts at 0. The commands also mean something entirely different than you first thought:

;; * down X *increases* your aim by X units.
;; * up X *decreases* your aim by X units.
;; * forward X does two things:
;;     * It *increases* your horizontal position by X units.
;;     * It *increases* your depth by your aim multiplied by X.
(defn move-sub-better [input]
  (let [pos [0 0 0]]
    (->> (reduce (fn [a v]
                   (let [[dir amt] v]
                     (case dir
                       "forward" (-> a
                                     (update 0 + amt)
                                     (update 1 + (* (nth a 2) amt)))
                       "down" (update a 2 + amt)
                       "up" (update a 2 - amt))))
                 pos
                 input)
         (drop-last)
         (apply *))))

(move-sub-better day2-input)

;; ---

;; #### S-expr solution to the naive case (which actually doesn't suck)
;; Borkdude half-joked...
;; > I considered making the most hacky version for 2 day by rewriting the input to s-expressions,
;; > `(forward 2)`, etc and then making functions for `forward`, etc so I could just run the program. ;)
(def day2-sexpr (->> (utils/input->seq "coyotesqrl/2021/day2-input.txt")
                     (map #(str "(" % ")"))))

(defn forward [x]
  (fn [[hor ver]]
    [(+ hor x) ver]))

(defn up [x]
  (fn [[hor ver]]
    [hor (- ver x)]))

(defn down [x]
  (fn [[hor ver]]
    [hor (+ ver x)]))

(let [pos [0 0]]
  (->> (reduce (fn [a v]
                 ((eval (read-string v)) a))
               pos
               day2-sexpr)
       (apply *)))
