;; ## Advent of Code 2022
;; ### [Day 5](https://adventofcode.com/2022/day/5)
;; [Link to code](https://github.com/coyotesqrl/advent-of-code/blob/main/src/coyotesqrl/2022/day05.clj)

(ns coyotesqrl.2022.day05
  (:require
   [clojure.string :as str]
   [coyotesqrl.utils :as utils]
   [nextjournal.clerk :as clerk]))

;; #### Part 1
;;The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in
;; stacks of marked crates, but because the needed supplies are buried under many other **crates**, the crates need to
;; be rearranged.
;;
;;The ship has a **giant cargo crane** capable of moving crates between stacks. To ensure none of the crates get crushed
;; or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are
;; rearranged, the desired crates will be at the top of each stack.
;;
;;The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her
;; **which** crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.
;;
;;They do, however, have a drawing of the starting stacks of crates **and** the rearrangement procedure (your puzzle
;; input). For example:
;;```
;;    [D]
;;[N] [C]
;;[Z] [M] [P]
;; 1   2   3
;;
;;move 1 from 2 to 1
;;move 3 from 1 to 3
;;move 2 from 2 to 1
;;move 1 from 1 to 2
;;```
;;In this example, there are three stacks of crates. Stack 1 contains two crates: crate `Z` is on the bottom, and crate
;; `N` is on top. Stack 2 contains three crates; from bottom to top, they are crates `M`, `C`, and `D`. Finally, stack 3
;; contains a single crate, `P`.
;;
;;Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one
;; stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2
;; to stack 1, resulting in this configuration:
;;
;;```
;;[D]
;;[N] [C]
;;[Z] [M] [P]
;; 1   2   3
;;```
;;In the second step, three crates are moved from stack 1 to stack 3. Crates are moved **one at a time**, so the first
;; crate to be moved (`D`) ends up below the second and third crates:
;;
;;        [Z]
;;        [N]
;;    [C] [D]
;;    [M] [P]
;; 1   2   3
;;Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved **one at a time**, crate `C` ends
;; up below crate `M`:
;;```
;;        [Z]
;;        [N]
;;[M]     [D]
;;[C]     [P]
;; 1   2   3
;;```
;;Finally, one crate is moved from stack 1 to stack 2:
;;```
;;        [Z]
;;        [N]
;;        [D]
;;[C] [M] [P]
;; 1   2   3
;;```
;;The Elves just need to know **which crate will end up on top of each stack**; in this example, the top crates are `C`
;; in stack 1, `M` in stack 2, and `Z` in stack 3, so you should combine these together and give the Elves the message
;; **CMZ**.
;;
;;**After the rearrangement procedure completes, what crate ends up on top of each stack?**
;;
;; ---
;; First off, we're going to need to partition the two halves of the input data, the initial stack arrangement and the
;; ops list
^{::clerk/visibility {:result :hide}}
(def input
  (->> "coyotesqrl/2022/day5-input.txt"
       (utils/input->seq)
       (partition-by str/blank?)))

;; We'll need to be able to interpret the stack arrangement
^{::clerk/visibility {:result :hide}}
(def letter-set (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

^{::clerk/visibility {:result :hide}}
(defn- parse-stack [st]
  (let [parse-line (fn [l] (map (fn [e] (keep #(letter-set %) e)) l))
        st (reverse st)
        num-stacks (parse-long (str (last (first st))))
        st (rest st)
        st (->> st
                (map #(partition-all 4 %))
                (map parse-line))]
    {:st st :num-stacks num-stacks}))

;; In preparation for rotating the matrix, we'll need to right pad the rows
^{::clerk/visibility {:result :hide}}
(defn- right-pad-rows [num-stacks st]
  (letfn [(pad-line [l] (take num-stacks (concat l (repeat num-stacks nil))))]
    (->> st
         (map pad-line))))

^{::clerk/visibility {:result :hide}}
(defn- rotate-matrix [num-stacks st]
  (->> st
       (map #(map first %))
       (right-pad-rows num-stacks)
       (apply map list)))

;; On this final step in prepping the stack, we'll reverse the order for performance purposes
^{::clerk/visibility {:result :hide}}
(defn- prep-stack []
  (let [{:keys [st num-stacks]} (parse-stack (first input))]
    (->> st
         (rotate-matrix num-stacks)
         (map reverse)
         (mapv #(remove nil? %)))))

;; And of course we'll need to be able to parse the operations
^{::clerk/visibility {:result :hide}}
(defn- parse-ops []
  (->> (last input)
       (map #(re-matches #"move (\d+) from (\d+) to (\d+)" %))
       (map rest)
       (map #(map parse-long %))))

^{::clerk/visibility {:result :hide}}
(defn- perform-op [st op single-crate?]
  (let [[num from to] op
        from-st (get st (dec from))
        to-st   (get st (dec to))
        crates  (take num from-st)]
    (-> st
        (assoc (dec from) (drop num from-st))
        (assoc (dec to) (concat (if single-crate? (reverse crates) crates) to-st)))))

^{::clerk/visibility {:result :hide}}
(defn- perform-ops [st ops single-crate?]
  (loop [st  st
         ops ops]
    (if (empty? ops)
      st
      (recur (perform-op st (first ops) single-crate?) (rest ops)))))

(let [st  (prep-stack)
      ops (parse-ops)]
  (->> (perform-ops st ops true)
       (map first)
       (apply str)))

;; ---
;; #### Part 2
;;As you watch the crane operator expertly rearrange the crates, you notice the process isn't following your prediction.
;;
;;Some mud was covering the writing on the side of the crane, and you quickly wipe it away. The crane isn't a
;; CrateMover 9000 - it's a **CrateMover 9001**.
;;
;;The CrateMover 9001 is notable for many new and exciting features: air conditioning, leather seats, an extra cup
;; holder, and **the ability to pick up and move multiple crates at once**.
;;
;;Again considering the example above, the crates begin in the same configuration:
;;```
;;    [D]
;;[N] [C]
;;[Z] [M] [P]
;; 1   2   3
;;```
;;Moving a single crate from stack 2 to stack 1 behaves the same as before:
;;```
;;[D]
;;[N] [C]
;;[Z] [M] [P]
;; 1   2   3
;;```
;;However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates
;; **stay in the same order**, resulting in this new configuration:
;;```
;;        [D]
;;        [N]
;;    [C] [Z]
;;    [M] [P]
;; 1   2   3
;;```
;;Next, as both crates are moved from stack 2 to stack 1, they **retain their order** as well:
;;```
;;        [D]
;;        [N]
;;[C]     [Z]
;;[M]     [P]
;; 1   2   3
;;```
;;Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate `C` that gets moved:
;;```
;;        [D]
;;        [N]
;;        [Z]
;;[M] [C] [P]
;; 1   2   3
;;```
;;In this example, the CrateMover 9001 has put the crates in a totally different order: **MCD**.
;;
;; Before the rearrangement process finishes, update your simulation so that the Elves know where they should stand to be
;; ready to unload the final supplies. **After the rearrangement procedure completes, what crate ends up on top of each
;; stack?**
;;
;; ---
(let [st  (prep-stack)
      ops (parse-ops)]
  (->> (perform-ops st ops false)
       (map first)
       (apply str)))
