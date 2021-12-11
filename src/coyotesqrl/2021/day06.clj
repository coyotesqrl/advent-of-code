;; ## Advent of Code 2021
;; ### Day 6
(ns coyotesqrl.2021.day06
  (:require [coyotesqrl.utils :as utils]))

;; ##### Input
(def day6-input (utils/input->one-line-numeric "coyotesqrl/2021/day6-input.txt"))

;; #### Part 1
;; A massive school of glowing lanternfish swims past. They must spawn quickly to reach such large
;; numbers - maybe exponentially quickly? You should model their growth rate to be sure.
;;
;; Although you know nothing about this specific species of lanternfish, you make some guesses about
;; their attributes. Surely, each lanternfish creates a new lanternfish once every 7 days.
;;
;; However, this process isn't necessarily synchronized between every lanternfish - one lanternfish
;; might have 2 days left until it creates another lanternfish, while another might have 4. So, you
;; can model each fish as a single number that represents the number of days until it creates a new
;; lanternfish.
;;
;; Furthermore, you reason, a new lanternfish would surely need slightly longer before it's capable
;; of producing more lanternfish: two more days for its first cycle.
;; Find a way to simulate lanternfish. How many lanternfish would there be after 80 days?

;; ##### Massive failure
;; I did not see the solution that would scale for parts 1 and 2. I had optimized my solution as
;; far as I could take it and finally punted and took a hint from my friend Josh
;; _[his aoc2021 repo](https://github.com/jghiloni/aoc2021)_ about tracking the count of fish with
;; a certain number of days left. Inverting the problem thusly removed all the exponential
;; processing.
(defn one-day [fs]
  (letfn [(updater [o v] (if (nil? o) v (+' v o)))]
    (reduce (fn [a [k v]]
              (if (zero? k)
                (-> (update a 6 updater v)
                    (assoc 8 v))
                (update a (dec k) updater v)))
            {}
            fs)))

(defn offset->generation [x d]
  (loop [d d
         fs {x 1}]
    (if (zero? d)
      fs
      (recur (dec d) (one-day fs)))))

(defn generate-all-fish [in d]
  (let [freq (frequencies in)
        m (into {} (for [x [1 2 3 4 5]]
                     {x (->> (offset->generation x d)
                             vals
                             (apply +'))}))]
    (->> m
         (map (fn [[k v]] (*' v (get freq k 0))))
         (apply +'))))

(generate-all-fish day6-input 80)

;; ---
;; #### Part 2
;; How many lanternfish would there be after 256 days?
(generate-all-fish day6-input 256)
