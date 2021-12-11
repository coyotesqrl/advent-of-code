;; ## Advent of Code 2021
;; ### Day 11: Dumbo Octopus
(ns coyotesqrl.2021.day11
  (:require [coyotesqrl.utils :as utils]
            [clojure.math.numeric-tower :as math]))

;; ##### Input
(def day11-input (->> (utils/input->seq "coyotesqrl/2021/day11-input.txt")
                      (map #(map str %))
                      (map #(map (fn [v] (Integer/parseInt v)) %))
                      flatten))

;; #### Part 1
;; The energy level of each octopus is a value between 0 and 9. Here, the top-left octopus has an
;; energy level of 5, the bottom-right one has an energy level of 6, and so on.
;;
;; You can model the energy levels and flashes of light in steps. During a single step, the
;; following occurs:
;;
;; * First, the energy level of each octopus increases by 1.
;; * Then, any octopus with an energy level greater than 9 flashes. This increases the energy level
;;   of all adjacent octopuses by 1, including octopuses that are diagonally adjacent. If this
;;   causes an octopus to have an energy level greater than 9, it also flashes. This process
;;   continues as long as new octopuses keep having their energy level increased beyond 9. (An
;;   octopus can only flash at most once per step.)
;; * Finally, any octopus that flashed during this step has its energy level set to 0, as it used
;;   all of its energy to flash.
;;
;; Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps.
;; **How many total flashes are there after 100 steps?**

;; ##### I hate grids; let's not use a grid
;; A sign of my lack of foresight. My initial solution worked over the very small test data provided
;; but then was off just a tad against the larger sample data. I looked at a couple of grid
;; progressions and realized that I was having issues on the east and west edges. Because the
;; `check-edges` anonymous fn here didn't exist. I was excluding neighbors outside the range of all
;; octopuses, but I was incorrectly considering, e.g. the octopus at the end of row 7 to be a
;; neighbor of the octupus at the start of row 8. 🤦
(defn- k->neighbors
  "Find the neighboring octopuses. They may be on the diagonal as well.
  Note: because of the choice to operate in 1D space, this is much uglier than it would have been."
  [width]
  (let [check-edges (fn [k s]
                      (case (mod k width)
                        0 (> 9 (mod s width))
                        9 (not (zero? (mod s width)))
                        true))
        size (* width width)
        below (range (dec width) (+ 2 width))
        above (map #(* -1 %) below)
        xs    (-> [-1 1] (into below) (into above))]
    (fn [k]
      (for [x xs
            :let [s (+ x k)]
            :when (and (<= 0 s) (> size s) (check-edges k s))]
        s))))

(defn sub-step-1
  "Increment all octopus energy levels."
  [m]
  (mapv inc m))

(defn sub-step-2-a
  "Checks for flashed octopuses, adding to the all-flashed set and sub-flashed seq."
  [m flashed]
  (reduce-kv (fn [{:keys [flashed sub-flashed] :as a} k v]
               (if (and (< 9 v) (not (flashed k)))
                 {:flashed (conj flashed k) :sub-flashed (conj sub-flashed k)}
                 a))
             {:flashed flashed :sub-flashed '()}
             m))

(defn sub-step-2-b
  "Increments neighbors of sub-step flashers."
  [m sub-flashed]
  (let [f (k->neighbors (math/sqrt (count m)))
        neighbors (->> sub-flashed
                       (map f)
                       flatten
                       vec)]
    (reduce (fn [a v]
              (update a v inc))
            m
            neighbors)))

(defn sub-step-2 [m flashed]
  (let [{:keys [flashed sub-flashed]} (sub-step-2-a m flashed)]
    {:m (sub-step-2-b m sub-flashed) :flashed flashed}))

(defn octupus->ground-state
  "Ground-states energy for any octopus that flashed."
  [m]
  (->> m
       (mapv #(if (> % 9) 0 %))))

(defn step [m]
  (let [m (sub-step-1 m)]
    (loop [prev-flashed #{}
           {:keys [m flashed]} (sub-step-2 m #{})]
      (if (= (count prev-flashed) (count (set flashed)))
        (octupus->ground-state m)
        (recur (set flashed) (sub-step-2 m flashed))))))

(defn process [m runs]
  (loop [n 0
         in m
         flash-count 0]
    (if (= n runs)
      {:m in :flash-count flash-count}
      (let [in (step in)
            flash-count (+ flash-count (->> in (filter zero?) count))]
        (recur (inc n) in flash-count)))))

(:flash-count (process day11-input 100))

;; ---
;; #### Part 2
;; It seems like the individual flashes aren't bright enough to navigate. However, you might have a
;; better option: the flashes seem to be synchronizing!
;;
;; If you can calculate the exact moments when the octopuses will all flash simultaneously, you
;; should be able to navigate through the cavern. **What is the first step during which all octopuses
;; flash?**

(defn part2-process [m]
  (loop [in m
         n 0
         flash-count 0]
    (if (= flash-count (count m))
      n
      (let [in (step in)
            flash-count (->> in (filter zero?) count)]
        (recur in (inc n) flash-count)))))

(part2-process day11-input)
