;; ## Advent of Code 2021
;; ### Day 10: Syntax Scoring
(ns coyotesqrl.2021.day10
  (:require [coyotesqrl.utils :as utils]))

;; ##### Input
(def day10-input (utils/input->seq "coyotesqrl/2021/day10-input.txt"))

;; #### Part 1
;; The navigation subsystem syntax is made of several lines containing chunks. There are one or more
;; chunks on each line, and chunks contain zero or more other chunks. Adjacent chunks are not
;; separated by any delimiter; if one chunk stops, the next chunk (if any) can immediately start.
;; Every chunk must open and close with one of four legal pairs of matching characters:
;;
;; * If a chunk opens with (, it must close with ).
;; * If a chunk opens with [, it must close with ].
;; * If a chunk opens with {, it must close with }.
;; * If a chunk opens with <, it must close with >.
;;
;; So, `()` is a legal chunk that contains no other chunks, as is `[]`. More complex but valid chunks
;; include `([])`, `{()()()}`, `<([{}])>`, `[<>({}){}[([])<>]]`, and even `(((((((((())))))))))`.
;;
;; Some lines are incomplete, but others are corrupted.
;; Stop at the first incorrect closing character on each corrupted line.
;; To calculate the syntax error score for a line, take the first illegal character on the line and
;; look it up in the following table:
;;
;; * ): 3 points.
;; * ]: 57 points.
;; * }: 1197 points.
;; * \>: 25137 points.
;;
;; Find the first illegal character in each corrupted line of the navigation subsystem. What is the
;; total syntax error score for those errors?

(def matching {"[" "]"
               "(" ")"
               "{" "}"
               "<" ">"})

(def open-braces (set (keys matching)))

(def syn-score {")" 3
                "]" 57
                "}" 1197
                ">" 25137})

(defn- bad-line [l]
  (reduce (fn [a v]
            (if (open-braces v)
              (conj a v)
              (if (not= v (get matching (peek a)))
                (reduced v)
                (pop a))))
          (list)
          l))

(defn score-input [in]
  (->> in
       (map #(map str %))
       (map bad-line)
       (remove seq?)
       (map syn-score)
       (apply +)))

(score-input day10-input)

;; ---
;; #### Part 2
;; Now, discard the corrupted lines. The remaining lines are incomplete.
;;
;; Incomplete lines don't have any incorrect characters - instead, they're missing some closing
;; characters at the end of the line. To repair the navigation subsystem, you just need to figure
;; out the sequence of closing characters that complete all open chunks in the line.
;;
;; You can only use closing characters (), ], }, or >), and you must add them in the correct order
;; so that only legal pairs are formed and all chunks end up closed.
;; Did you know that autocomplete tools also have contests? It's true! The score is determined by
;; considering the completion string character-by-character. Start with a total score of 0. Then,
;; for each character, multiply the total score by 5 and then increase the total score by the point
;; value given for the character in the following table:
;;
;; * ): 1 point.
;; * ]: 2 points.
;; * }: 3 points.
;; * \>: 4 points.
;;
;; So, the last completion string above - `])}>` - would be scored as follows:
;;
;; * Start with a total score of 0.
;; * Multiply the total score by 5 to get 0, then add the value of ] (2) to get a new total score of 2.
;; * Multiply the total score by 5 to get 10, then add the value of ) (1) to get a new total score of 11.
;; * Multiply the total score by 5 to get 55, then add the value of } (3) to get a new total score of 58.
;; * Multiply the total score by 5 to get 290, then add the value of > (4) to get a new total score of 294.
;;
;; Autocomplete tools are an odd bunch: the winner is found by sorting all of the scores and then
;; taking the middle score. (There will always be an odd number of scores to consider.)

(def match-score {"(" 1
                  "[" 2
                  "{" 3
                  "<" 4})

(defn score-completion [l]
  (reduce (fn [a v]
            (->> a
                 (* 5)
                 (+ v)))
          0
          l))

(defn complete-command [in]
  (let [scores (->> in
                    (map #(map str %))
                    (map bad-line)
                    (filter seq?)
                    (map #(map match-score %))
                    (map score-completion)
                    sort)
        mid-minus-1 (quot (count scores) 2)]
    (->> scores
         (drop mid-minus-1)
         (first))))

(complete-command day10-input)
