;; ## Advent of Code 2021
;; ### Day 14: Extended Polymerization
(ns coyotesqrl.2021.day14
  (:require [coyotesqrl.utils :as utils]
            [clojure.set :as set]))

;; ##### Input
(def day14-input (utils/input->seq "coyotesqrl/2021/day14-input.txt"))

;; #### Part 1
;; The submarine manual contains instructions for finding the optimal polymer formula; specifically,
;; it offers a polymer template and a list of pair insertion rules (your puzzle input). You just
;; need to work out what polymer would result after repeating the pair insertion process a few
;; times.
;;
;; The first line is the polymer template - this is the starting point of the process.
;;
;; The following section defines the pair insertion rules. A rule like AB -> C means that when
;; elements A and B are immediately adjacent, element C should be inserted between them. These
;; insertions all happen simultaneously.
;;
;; > Note that these pairs overlap: the second element of one pair is the first element of the next
;;   pair. Also, because all pairs are considered simultaneously, inserted elements are not
;;   considered to be part of a pair until the next step.
;;
;; Apply 10 steps of pair insertion to the polymer template and find the most and least common
;; elements in the result. **What do you get if you take the quantity of the most common element and
;; subtract the quantity of the least common element?**

(defn- generate-rule-map [in]
  (into {} (->> in
                rest
                rest
                (map #(re-seq #"(\w\w) -> (\w)" %))
                (map flatten)
                (map (fn [[_ k v]] [k v])))))

(defn- apply-rule [rule-set pair]
  (str (first pair) (get rule-set pair)))

;; **Note that this is a naive process, and it was certain to fall over on part 2.**
(defn- process
  ([in] (process in 1))
  ([in n]
   (let [rules (generate-rule-map in)
         run-rule (partial apply-rule rules)]
     (loop [n n
            in (first in)]
       (if (zero? n)
         in
         (recur (dec n) (str (->> in
                                  (partition 2 1)
                                  (map #(apply str %))
                                  (map run-rule)
                                  (apply str))
                             (last in))))))))

(defn ->polymer [in n]
  (let [freqs (->> (process in n)
                   (frequencies)
                   (set/map-invert)
                   sort)]
    (- (first (last freqs)) (ffirst freqs))))

(time (->polymer day14-input 10))

;; ---
;; #### Part 2
;; The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run
;; more steps of the pair insertion process; a total of 40 steps should do it.
;;
;; Instead of naively processing, we'll instead keep a count of the number of pairs at each step.
;; Using the provided sample input:
;; ```
;; NNCB
;;
;; CH -> B
;; HH -> N
;; CB -> H
;; NH -> C
;; HB -> C
;; HC -> B
;; HN -> C
;; NN -> C
;; BH -> H
;; NC -> B
;; NB -> B
;; BN -> B
;; BB -> N
;; BC -> B
;; CC -> N
;; CN -> C
;; ```
;; We begin with a frequency map that looks like `{[\N \N] 1, [\N \C] 1, [\C \B] 1}`
;; at each step, we update the frequency map by finding the _two_ pairs that are mapped to each of
;; the input pairs and adding that count to the map. In this first step, we find that the pair
;; `[\N \N]` would map to `[[\N \C] [\C \N]]` (the effect of inserting a `\C` between the two `\N`s).
(defn- generate-mappings [in]
  (into {} (->> in
                rest
                rest
                (map #(re-seq #"(\w\w) -> (\w)" %))
                (map flatten)
                (map (fn [[_ k v]] [(vec k)
                                    [[(first k) (first v)]
                                     [(first v) (last k)]]])))))

(defn- +-nil
  "Helper fn to add to a value or return it if nil"
  [x n]
  (if x
    (+ x n)
    n))

(defn- step
  "Processes a single step of insertions, returning the updated frequency map."
  [in mappings]
  (reduce (fn [a [pair cnt]]
            (let [[left right] (get mappings pair)]
              (-> a
                  (update left #(+-nil % cnt))
                  (update right #(+-nil % cnt)))))
          {}
          in))

(defn- run-steps
  "Processes n step-wise insertion operations."
  [in mappings n]
  (loop [n n
         in in]
    (if (zero? n)
      in
      (recur (dec n) (step in mappings)))))

(defn- pair-freqs->letter-freqs
  "Using the frequencies of the pairs, determine the frequencies of each letter.
  The last letter of the polymer template needs to be incremented (here, pre-set to 1)
  in order to account for the manner in which we count letter frequencies. Because we
  always count the first letter of each pair, we need to handle the special case of
  the terminal letter."
  [in last-ltr]
  (reduce (fn [a [pair cnt]]
            (update a (first pair) #(+-nil % cnt)))
          {last-ltr 1}
          in))

(defn part2 [in n]
  (let [last-ltr (->> in first last)
        init (->> in
                  first
                  (partition 2 1)
                  frequencies)
        mappings (generate-mappings in)
        freqs (-> init
                  (run-steps mappings n)
                  (pair-freqs->letter-freqs last-ltr)
                  (set/map-invert)
                  sort)]
    (tap> {:freqs freqs :last-ltr last-ltr})
    (- (first (last freqs)) (ffirst freqs))))

(time (part2 day14-input 40))
