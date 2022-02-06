;; ## Advent of Code 2021
;; ### Day 18: Snailfish
(ns coyotesqrl.2021.day18
  (:require [coyotesqrl.utils :as utils]
            [clojure.zip :as zip]
            [instaparse.core :as insta]
            [rewrite-clj.zip :as z]))

;; ##### Input

;; #### Part 1
; expr = (mul | add)
;mul = (term | mul | add) <sp> <'*'> <sp> term
;add = (term | add | mul) <sp> <'+'> <sp> term
;<term> = number | group
;<group> = <'('> expr <')'>
;<sp> = <#'[ ]+'>
;number = #'[0-9]+'

;(def transform-options
;  {:add    +
;   :mul    *
;   :number clojure.edn/read-string
;   :expr   identity})
(def snail-grammar
  "pair = <ob> (number | pair) <comma> (number | pair) <cb>
  <sp> = <#'[ ]+'>
  <comma> = #'[,]'
  <ob> = #'\\['
  <cb> = #'\\]'
  number = #'[0-9]+'")

(def transform-options
  {:add    vector
   :pair   vector
   :number clojure.edn/read-string
   :expr   identity})

(def snail-foo (insta/parser snail-grammar))
(snail-foo "[[1,2],3]")
(snail-foo "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")
(snail-foo "[1,2] + [[3,4],5]")


(snail-foo "[[[[[9,8],1],2],3],4]")
(snail-foo "[7,[6,[5,[4,[3,2]]]]]")

(->> "[1,2] + [[3,4],5]"
     snail-foo
     (insta/transform transform-options))
; (->> (new-math input) (insta/transform transform-options))

(defn explode [v])

(defn split [v])

(defn add [l r]
  (conj [l] r))

(reduce (fn [a v]
          (add a v))
        '([1 1] [2 2] [3 3] [4 4]))


(def snail-two
  (->>
    "pair = <ob> (number | pair) <sp> (number | pair) <cb>
     <sp> = <#'[ ]+'>
     <ob> = #'\\['
     <cb> = #'\\]'
     number = #'[0-9]+'"
    insta/parser))

(->> (snail-two (str (add [[[[4, 3], 4], 4], [7, [[8, 4], 9]]]
                          [1, 1])))
     zip/vector-zip
     zip/down
     zip/right)

(def explicit-grammar
  (->>
    "
    pair = <ob> (number | bignumber | pair) <sp> (number | bignumber | pair) <cb>
    split-pair = (<ob> number <sp> number number <cb>) | (<ob> number number <sp> number <cb>) | (<ob> number number <sp> number number <cb>)
    <sp> = <#'[,]+'>
    <ob> = #'\\['
    <cb> = #'\\]'
    number = #'[0-9]'
    bignumber = #'[0-9][0-9]'"
    insta/parser))

(defn- split [[l r]]
  (letfn [(n->sp [n]
            (let [l (quot n 2)
                  r (- n l)]
              [l r]))]
    (cond
      (and (>= l 10) (< r 10)) [(n->sp l) r]
      (and (>= r 10) (< l 10)) [l (n->sp r)]
      :else [(n->sp l) (n->sp r)])))

(defn n->sp [n]
  (let [n (Integer/parseInt n)
        l (quot n 2)
        r (- n l)]
    [l r]))
;[(str l) (str r)]))

(->> (explicit-grammar "[[[[0,7],4],[15,[0,13]]],[1,1]]")
     (insta/transform {:number    #(Integer/parseInt %)
                       :bignumber n->sp
                       :pair      vector}))


(def foobar (add [[[[4, 3], 4], 4], [7, [[8, 4], 9]]]
                 [1, 1]))

(defn left-wrapped [z]
  (loop [n 4
         z z]
    (if (or (zero? n) (zip/end? z))
      z
      (recur (dec n) (zip/next z)))))

#_(let [z (left-wrapped foobar)]
    (if (vector? (zip/node z))
      (zip/prev z)
      100))


(defn- exp-right
  [n]
  (let [v (first n)
        [[_ l2] r] v]
    (assoc n 0 [0 (+ l2 r)])))

(defn- exp-left
  [n]
  (let [v (first n)
        [l [r1 _]] v]
    (assoc n 0 [(+ l r1) 0])))

(defn- explode [f z]
  (-> z
      z/up
      (z/subedit-node f)))


(defn- test-explode-right [v]
  (->> v
       zip/vector-zip
       zip/down
       zip/down
       zip/down
       zip/down
       (explode exp-right)
       zip/root))

(defn- test-explode-left [v]
  (->> v
       zip/vector-zip
       zip/down
       zip/right
       zip/down
       zip/right
       zip/down
       zip/right
       zip/down
       zip/right
       (explode exp-left)
       zip/root))

(test-explode-right [[[[[9,8],1],2],3],4])
(test-explode-left [7,[6,[5,[4,[3,2]]]]])
#_(test-explode-left [[6,[5,[4,[3,2]]]],1])

(->> [7,[6,[5,[4,[3,2]]]]]
     zip/vector-zip
     zip/down
     zip/right
     zip/down
     zip/right
     zip/down
     zip/right
     zip/down
     zip/right
     zip/node)







(->> (add [[[[4, 3], 4], 4], [7, [[8, 4], 9]]]
          [1, 1])
     zip/vector-zip
     zip/next
     zip/next
     zip/next
     zip/next
     zip/next)
;zip/end?)

(defn- left->0 [])

(defn- left-add [v {:keys [acc l-addend] :as a}]
  (let [pared-str (->> acc
                       reverse
                       (drop-while #(= \[ %))
                       (drop-while #(not= \[ %))
                       reverse
                       (apply str))]
    (-> a
        (assoc :acc (str pared-str (+ v l-addend) ","))
        (assoc :l-addend nil))))

;; completely manual
(defn foobar [x]
  (reduce (fn [a v]
            (cond->> a
                     (= 5 (:ob-cnt a)) ()
                     (= \[ v) (update :ob-cnt inc)
                     (= \] v) (update :ob-cnt dec))


            (tap> v))
          {:ob-cnt     0
           :l-addend   nil
           :r-addend   nil
           :add-right? nil
           :acc        ""}
          x))

(comment
  (foobar "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))

;; ---
;; #### Part 2
