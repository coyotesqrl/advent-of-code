;; ## Advent of Code 2021
;; ### Day 5
(ns coyotesqrl.2021.day5
  (:require [coyotesqrl.utils :as utils]
            [clojure.string :as str]))

;; ##### Input
(def day5-input
  (let [ptstr->pts (fn [pt] (map #(Long/parseLong %) (str/split pt #",")))]
    (->> (utils/input->seq "coyotesqrl/2021/day5-input.txt")
         (map #(re-matches #"(\d+,\d+)[^\d]*(\d+,\d+)" %))
         (map rest)
         (map (fn [[st en]] [(ptstr->pts st) (ptstr->pts en)])))))

;; #### Part 1
;; You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce
;; large, opaque clouds, so it would be best to avoid them if possible.
;;
;; They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents
;; (your puzzle input) for you to review.
;;
;; Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the
;; coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These
;; line segments include the points at both ends.
;;
;; For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.
;;
;; To avoid the most dangerous areas, you need to determine the number of points where at least two
;; lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total
;; of 5 points.
;;
;; Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

(defn in->hor-and-ver-only [in]
  (->> in
       (filter (fn [[[st-x st-y] [en-x en-y]]] (or (= st-x en-x) (= st-y en-y))))))

;; Turns out, I'm just slow. Instead of all this, could have used `range` for all cases. I had
;; initially used it for the degenerate horizontal and vertical cases but not for the diagonal. All
;; I had to do with the diagonal was invoke
;; `(map vector x-range y-range)`
(defn pts->line [[[st-x st-y] [en-x en-y]]]
  (let [move-fn (fn [st en] (cond
                              (= st en) identity
                              (> st en) dec
                              :else     inc))
        x-fn (move-fn st-x en-x)
        y-fn (move-fn st-y en-y)]
    (loop [x st-x
           y st-y
           res (list [st-x st-y])]
      (let [x (x-fn x)
            y (y-fn y)
            res (conj res [x y])]
        (if (and (= x en-x) (= y en-y))
          res
          (recur x y res))))))

(defn map-vents [in]
  (letfn [(on-vent [m l]
            (reduce (fn [a v]
                      (update a v #(if (nil? %) 1 (inc %))))
                    m
                    l))]

    (reduce (fn [a v] (on-vent a (pts->line v)))
            {}
            in)))

(->> (map-vents (in->hor-and-ver-only day5-input))
     (filter (fn [r] (>= (val r) 2)))
     count)

;; ---
;; #### Part 2
;; Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture;
;; you need to also consider diagonal lines.
;;
;; Because of the limits of the hydrothermal vent mapping system, the lines in your list will only
;; ever be horizontal, vertical, or a diagonal line at exactly 45 degrees.
;;
;; You still need to determine the number of points where at least two lines overlap.
(->> (map-vents day5-input)
     (filter (fn [r] (>= (val r) 2)))
     count)
