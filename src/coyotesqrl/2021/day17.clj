;; ## Advent of Code 2021
;; ### Day 17: Trick Shot
(ns coyotesqrl.2021.day17
  (:require [coyotesqrl.utils :as utils]
            [clojure.java.io :as io]))

;; ##### Input
(def target-area (->> "coyotesqrl/2021/day17-input.txt"
                      io/resource
                      slurp
                      (re-matches #"target area:\s*x=([-]?\d+)..([-]?\d+),\s*y=([-]?\d+)..([-]?\d+)[^\d]*")
                      rest
                      (partition 2)
                      (mapv #(mapv (fn [v] (Integer/parseInt v)) %))))

;; #### Part 1
;; Ahead of you is what appears to be a large ocean trench. Could the keys have fallen into it?
;; You'd better send a probe to investigate.
;;
;; The probe launcher on your submarine can fire the probe with any integer velocity in the
;; x (forward) and y (upward, or downward if negative) directions. For example, an initial x,y
;; velocity like 0,10 would fire the probe straight up, while an initial velocity like 10,-1 would
;; fire the probe forward at a slight downward angle.
;;
;; The probe's x,y position starts at 0,0. Then, it will follow some trajectory by moving in steps.
;; On each step, these changes occur in the following order:
;;
;; * The probe's x position increases by its x velocity.
;; * The probe's y position increases by its y velocity.
;; * Due to drag, the probe's x velocity changes by 1 toward the value 0; that is, it decreases by 1
;;   if it is greater than 0, increases by 1 if it is less than 0, or does not change if it is
;;   already 0.
;; * Due to gravity, the probe's y velocity decreases by 1.
;;
;; For the probe to successfully make it into the trench, the probe must be on some trajectory that
;; causes it to be within a target area after any step. The submarine computer has already
;; calculated this target area (your puzzle input).
;;
;; If you're going to fire a highly scientific probe out of a super cool probe launcher, you might
;; as well do it with style. How high can you make the probe go while still reaching the target
;; area?

(defn in-range [[[tx1 tx2] [ty1 ty2]] [px py]]
  (let [min-x (min tx1 tx2)
        min-y (min ty1 ty2)
        max-x (max tx1 tx2)
        max-y (max ty1 ty2)]
    (and (<= min-x px max-x) (<= min-y py max-y))))

(defn step [[[px py] [vx vy]]]
  [[(+ px vx) (+ py vy)]
   [(+ vx (cond
            (zero? vx) 0
            (pos? vx) -1
            :else 1)) (dec vy)]])

;; > I was sure I had this right at first, but I had neglected to account for the drag on x velocity
;;   potentially causing the state where the probe started left or right of the target area but
;;   never crossed it. Those really need to be my first two test conditions, as we could start
;;   below the target area and shoot a high arc above it. But that led to the case where x velocity
;;   dropped to zero, and we plummeted downward without ever thinking we'd overshot.
(defn- overshot-fn
  [[[tx1 tx2] [ty1 ty2]] [[px py] _]]
  (let [min-x (min tx1 tx2)
        min-y (min ty1 ty2)
        max-x (max tx1 tx2)
        max-y (max ty1 ty2)]
    (fn [[[p'x p'y] [_ v'y]]]
      (cond
        (and (neg? v'y) (< p'y min-y)) true
        (< px min-x) (> p'x max-x)
        (> px max-x) (< p'x min-x)
        (< py min-y) (> p'y max-y)
        :else (< p'y min-y)))))

(defn check-velocity [target p]
  (let [overshot (overshot-fn target p)
        step-pts (take-while #(not (or (overshot %)
                                       (in-range target (first %))))
                             (iterate #(step %) p))
        final-step (step (last step-pts))]
    (when (->> final-step
               first
               (in-range target))
      (->> (cons final-step step-pts)
           (map first)
           (map second)
           (apply max)
           (assoc {:velocity (second p)} :max-height)))))

(defn find-max-height [target]
  (->> (for [vx (range -500 500)
             vy (range -500 500)
             :let [check (check-velocity target [[0 0] [vx vy]])]
             :when (some? check)]
         check)
       (sort-by :max-height)
       last
       :max-height))

;; **What is the highest y position it reaches on this trajectory?**
(->> target-area
     find-max-height
     utils/answer-block)

;; ---
;; #### Part 2
;; Maybe a fancy trick shot isn't the best idea; after all, you only have one probe, so you had
;; better not miss.
;;
;; To get the best idea of what your options are for launching the probe, you need to find every
;; initial velocity that causes the probe to eventually be within the target area after any step.

;; > Yep. Brute force. One million initial velocities tested, their ranges set empirically. I am
;;   quite certain I could test a much smaller subset, or skip the brute force entirely in favor of
;;   actual math; however...it works; and it's only about a minute to process the million inputs
;;   here, and in its sister fn for Part 1.
(defn count-valid-velocities [target]
  (->> (for [vx (range -500 500)
             vy (range -500 500)
             :let [check (check-velocity target [[0 0] [vx vy]])]
             :when (some? check)]
         check)
       count))

;; **How many distinct initial velocity values cause the probe to be within the target area after
;; any step?**
(->> target-area
     count-valid-velocities
     utils/answer-block)
