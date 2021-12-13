;; ## Advent of Code 2021
;; ### Day 13: Transparent Origami
(ns coyotesqrl.2021.day13
  (:require [coyotesqrl.utils :as utils]
            [clojure.string :as str]))

;; ##### Input
(defn- max-extents-easy [folds]
  (letfn [(find-max [axis]
            (->> folds
                 (map #(re-matches (re-pattern (format "^fold along %s=(\\d+)" axis)) %))
                 (remove nil?)
                 (map second)
                 (map #(Integer/parseInt %))
                 (apply max)
                 (* 2)
                 inc))]
    {:max-x (find-max "x") :max-y (find-max "y")}))

(defn- pts->grid [folds pts]
  (let [pts (->> pts
                 (map #(str/split % #","))
                 (map #(map (fn [v] (Integer/parseInt v)) %)))
        {:keys [max-x max-y]} (max-extents-easy folds)]
    (reduce (fn [a [x y]]
              (assoc-in a [y x] 1))
            (vec (repeat max-y (vec (repeat max-x 0))))
            pts)))

(defn- read-origami [f]
  (let [[pts folds] (->> (utils/input->seq f)
                         (partition-by #(str/starts-with? % "fold")))
        pts (->> pts (remove str/blank?) (pts->grid folds))
        folds (map #(str/replace % #"^fold along ([x|y])=(\d+)" "(fold-$1 $2)") folds)]
    {:pts pts :folds folds}))

(def day13-input (read-origami "coyotesqrl/2021/day13-input.txt"))

;; #### Part 1
;; You reach another volcanically active part of the cave. It would be nice if you could do some
;; kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.
;;
;; Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you
;; are greeted with:
;;
;; Congratulations on your purchase! To activate this infrared thermal imaging
;; camera system, please enter the code found on page 1 of the manual.
;; Apparently, the Elves have never used this feature. To your surprise, you manage to find the
;; manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The
;; transparent paper is marked with random dots and includes instructions on how to fold it up
;; (your puzzle input).
;;
;; The first section is a list of dots on the transparent paper. 0,0 represents the top-left
;; coordinate. The first value, x, increases to the right. The second value, y, increases downward.
;; So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0.
;;
;; Then, there is a list of fold instructions. Each instruction indicates a line on the transparent
;; paper and wants you to fold the paper up (for horizontal y=... lines) or left
;; (for vertical x=... lines).
;;
;; **How many dots are visible after completing just the first fold instruction on your transparent
;; paper?**

(defn fold-x
  "Evaluator for fold-x instructions."
  [n]
  (fn [l]
    (->> l
         (map #(vector (take n %) (reverse (take-last n %))))
         (map (fn [[a b]] (map bit-or a b))))))

(defn fold-y
  "Evaluator for fold-y instructions."
  [n]
  (fn [l]
    (let [top (take n l)
          bot (reverse (take-last n l))]
      (map (fn [a b] (map bit-or a b)) top bot))))

(defn process-first-fold [in]
  (let [first-fold (eval (read-string (first (:folds in))))]
    (->> in
         :pts
         first-fold
         flatten
         (apply +))))

(process-first-fold day13-input)

;; ---
;; #### Part 2
;; Finish folding the transparent paper according to the instructions. The manual says the code is
;; always eight capital letters.
;;
;; **What code do you use to activate the infrared thermal imaging camera system?**

(defn in->code [in]
  (let [folded (reduce (fn [a v]
                         ((eval (read-string v)) a))
                       (:pts in)
                       (:folds in))]
    (->> folded
         (map #(map (fn [v] (if (zero? v) "\u2591" "\u2588")) %))
         (map #(apply str %)))))

(in->code day13-input)
