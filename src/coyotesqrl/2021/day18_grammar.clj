(ns coyotesqrl.2021.day18-grammar
  (:require [coyotesqrl.utils :as utils]
            [instaparse.core :as insta]

            [clojure.test :refer [deftest is]]))


;     exp-left  = exp-ll-neigh? (<ob> <ob> <ob> <ob> exp-ll <sp> exp-lr <cb>) <sp> exp-lr-addend?
(def explicit-grammar
  (->>
    "
    input = pair | add
    pair = <ob> (x-number | pair) <sp> (x-number | pair) <cb>
    add = pair <plus> pair

    exp-left  = (exp-ll-addend <cb>)? (<ob> <ob> <ob> <ob> exp-ll <sp> exp-lr <cb>) <sp> exp-lr-addend?
    exp-right = exp-rr-addend? (<ob> exp-rl <sp> exp-rr <cb> <cb> <cb> <cb>) <sp> exp-rr-neigh?

    exp-ll = x-number
    exp-lr = x-number
    exp-rl = x-number
    exp-rr = x-number

    exp-ll-neigh = (exp-ll-addend <cb> <sp>)
    exp-ll-addend = x-number
    exp-lr-addend = x-number

    exp-rl-addend = x-number
    exp-rr-neigh = (<sp> <ob> exp-rr-addend)
    exp-rr-addend = x-number

    <sp> = <#'[,]+'> | <#'[ ]+'>
    <ob> = #'\\['
    <cb> = #'\\]'
    x-number = number | bignumber
    number = #'[0-9]'
    bignumber = #'[0-9][0-9]'
    plus = #'[+]'"
    insta/parser))

(defn exp-left [a b c d]
  (let [x (if a (+ a b) 0)
        y (if d (+ c d) 0)]
    [x y]))

(defn n->sp [n]
  (let [n (Integer/parseInt n)
        l (quot n 2)
        r (- n l)]
    [l r]))

(defn snail+ [l r]
  (conj (conj [] l) r))

(->> (explicit-grammar "[[[[0,7],4],[15,[0,13]]],[1,1]]")
     (insta/transform {:number    #(Integer/parseInt %)
                       :bignumber n->sp
                       :add       snail+
                       :pair      vector}))

(def parse-ops {:number    #(Integer/parseInt %)
                :bignumber n->sp
                :x-number  identity
                :exp-left  exp-left
                :add       snail+
                :pair      vector
                :input     identity})

(defn xform [in]
  (->> (explicit-grammar in)
       (insta/transform parse-ops)))

(str (xform "[[[[4,3],4],4],[7,[[8,4],9]]]+[1,1]"))
(xform "[[[[0,7],4],[15,[0,13]]],[1,1]]")
(xform "[[[[[9,8],1],2],3],4]")

(deftest test-addition
  (is (= "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
         (str (xform "[[[[4,3],4],4],[7,[[8,4],9]]]+[1,1]")))))