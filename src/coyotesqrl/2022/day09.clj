;; ## Advent of Code 2022
;; ### [Day 9](https://adventofcode.com/2022/day/9)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2022/day09.clj)

(ns coyotesqrl.2022.day09
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;; _N.B. The length of today's problem description (in particular the graphs of knot movement) was so
;; long, that I opted not to include it here. It can, of course, be viewed by clicking through the link above._
;;
;; ---

^{::clerk/visibility {:result :hide}}
(def input (->> "coyotesqrl/2022/day9-input.txt"
                (utils/input->seq)))

^{::clerk/visibility {:result :hide}}
(defn- x-touching? [head tail]
  (<= -1 (- (first head) (first tail)) 1))

^{::clerk/visibility {:result :hide}}
(defn- y-touching? [head tail]
  (<= -1 (- (second head) (second tail)) 1))

^{::clerk/visibility {:result :hide}}
(defn- touching? [head tail]
  (and (x-touching? head tail)
       (y-touching? head tail)))

^{::clerk/visibility {:result :hide}}
(defn- move-tail-xwards [[hx _hy] tail]
  (cond
    (= hx (first tail)) tail
    (> hx (first tail)) (update tail 0 inc)
    :else (update tail 0 dec)))

^{::clerk/visibility {:result :hide}}
(defn- move-tail-ywards [[_hx hy] tail]
  (cond
    (= hy (second tail)) tail
    (> hy (second tail)) (update tail 1 inc)
    :else (update tail 1 dec)))

^{::clerk/visibility {:result :hide}}
(defn- move-tail [head tail]
  (if (touching? head tail)
    tail
    (->> tail
         (move-tail-xwards head)
         (move-tail-ywards head))))

^{::clerk/visibility {:result :hide}}
(defn- move-head [head dir]
  (case dir
    "R" (update head 0 inc)
    "L" (update head 0 dec)
    "U" (update head 1 inc)
    "D" (update head 1 dec)))

;; Yes. `move2`. Because there was an initial version, without the nested `reduce` call, that worked
;; only on the degenerate case of two knots. I opted to update it and re-verify the first part using it
;; prior to moving onto part 2; I further opted to delete the original, but I left the name as an
;; artifact left as a puzzle for digital archaeologists of the future.
;;
;; Please note this is a hideous solution. There may be a non-iterative solution that eludes me, but
;; this very much feels like a step-wise problem that cannot be solved another way. I opted for `reduce` for
;; my looping control, as it's the most "LISP-like" way, short of recursion. Also, that gave me the ability
;; to add the minimal optimization of calling `reduced`.
^{::clerk/visibility {:result :hide}}
(defn- move2 [op knots]
  (let [[dir steps] (str/split op #" ")]
    (reduce (fn [{:keys [knots] :as a} _v]
              (let [head       (move-head (first knots) dir)
                    step-knots (reduce (fn [a v]
                                         (let [prior (get a v)
                                               curr  (get a (inc v))]
                                           (if (touching? prior curr)
                                             (reduced a)
                                             (assoc a (inc v) (move-tail prior curr)))))
                                       (assoc (:knots a) 0 head)
                                       (range 0 (dec (count knots))))]
                (tap> {:op op :knots knots :step-knots step-knots :a a})
                (-> a
                    (assoc :knots step-knots)
                    (update :tails conj (last step-knots)))))

            {:knots (:knots knots) :tails (:tails knots)}
            (range 0 (parse-long steps)))))

(->> input
     (reduce (fn [a v]
               (move2 v a))
             {:knots [[0 0] [0 0]] :tails [[0 0]]})
     :tails
     distinct
     count
     (utils/answer-block))

;; ---
;; #### Part 2
;; See note above; go to original problem description on Advent of Code page.
;;
;; ---

(->> input
     (reduce (fn [a v]
               (move2 v a))
             {:knots (vec (repeat 10 [0 0])) :tails [[0 0]]})
     :tails
     distinct
     count
     (utils/answer-block))
