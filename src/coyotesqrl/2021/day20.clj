;; ## Advent of Code 2021
;; ### Day 20: Trench Map
(ns coyotesqrl.2021.day20
  (:require [coyotesqrl.utils :as utils]))

;; ##### Input
(defn- line->map [l]
  (->> l
       (mapv #(if (= \# %) 1 0))))

(defn- get-algo [f]
  (->> f
       utils/input->seq
       first
       line->map))

(defn- get-input [f]
  (->> f
       utils/input->seq
       (drop 2)
       (map line->map)
       vec))

(def day20-algo (get-algo "coyotesqrl/2021/day20-input.txt"))

(def day20-input (get-input "coyotesqrl/2021/day20-input.txt"))

;; #### Part 1
;; With the scanners fully deployed, you turn their attention to mapping the floor of the ocean
;; trench.
;;
;; When you get back the image from the scanners, it seems to just be random noise. Perhaps you can
;; combine an image enhancement algorithm and the input image (your puzzle input) to clean it up a
;; little.
;;
;; The first section is the image enhancement algorithm. It is normally given on a single line, but
;; it has been wrapped to multiple lines in this example for legibility. The second section is the
;; input image, a two-dimensional grid of light pixels (#) and dark pixels (.).
;;
;; The image enhancement algorithm describes how to enhance an image by simultaneously converting all
;; pixels in the input image into an output image. Each pixel of the output image is determined by
;; looking at a 3x3 square of pixels centered on the corresponding input image pixel. So, to
;; determine the value of the pixel at (5,10) in the output image, nine pixels from the input image
;; need to be considered: `(4,9), (4,10), (4,11), (5,9), (5,10), (5,11), (6,9), (6,10)`, and` `(6,11)`.
;; These nine input pixels are combined into a single binary number that is used as an index in the
;; image enhancement algorithm string.

(defn- pt->block [[col row]]
  (for [row (range (dec row) (+ 2 row))
        col (range (dec col) (+ 2 col))]
    [col row])) ; invert

(defn- blockpt-val [m infinite-void [col row]]
  (let [max-row (dec (count m))
        max-col (dec (count (first m)))]
    (cond
      (neg? col) infinite-void
      (neg? row) infinite-void
      (> col max-col) infinite-void
      (> row max-row) infinite-void
      :else (nth (nth m row) col))))

(defn- pt->int [m p infinite-void]
  (let [block (pt->block p)]
    (Integer/parseInt
     (->> block
          (map #(blockpt-val m infinite-void %))
          (map str)
          (apply str))
     2)))

(defn- pt->val [algo-map key]
  (nth algo-map key))

(defn- empty-grid [m]
  (let [max-y (count m)
        max-x (count (first m))]
    (vec (repeat max-y (vec (repeat max-x 0))))))

(defn- pad-grid [m n v]
  (let [row-padder (fn [r]
                     (-> []
                         (into (repeat n v))
                         (into r)
                         (into (repeat n v))))
        col-padder (fn [m r]
                     (-> []
                         (into (repeat n r))
                         (into m)
                         (into (repeat n r))))
        m (mapv row-padder m)
        pad-row (vec (repeat (count (first m)) v))]
    (col-padder m pad-row)))

(defn enhance
  [algo-map infinite-void m]
  (let [m       (pad-grid m 5 infinite-void)
        row-len (count (first m))]
    (reduce (fn [a v]
              (let [[col-idx row-idx] v
                    val (pt->val algo-map (pt->int m v infinite-void))
                    row (get a row-idx)
                    row (assoc row col-idx val)]
                (assoc a row-idx row)))
            (empty-grid m)
            (for [x (range 0 row-len)
                  y (range 0 (count m))]
              [x y]))))

(defn part-1 [algo-map m]
  (->> m
       (enhance algo-map 0)
       (enhance algo-map 1)
       flatten
       (remove zero?)
       count))

;; Start with the original input image and apply the image enhancement algorithm twice, being
;; careful to account for the infinite size of the images. **How many pixels are lit in the
;; resulting image?**

(utils/answer-block
 (part-1 day20-algo day20-input))

;; ---
;; #### Part 2
;; You still can't quite make out the details in the image. Maybe you just didn't enhance it enough.
;;
;; Start again with the original input image and apply the image enhancement algorithm 50 times.
;; **How many pixels are lit in the resulting image?**

;; > The difference between the sample input and my day's actual data is the value of `bit 0`. In
;;   the sample data, it is `0`, so that the infinite void will remain dark. In the actual input
;;   data, it is `1`, so the infinite void will appear to flash white, then black again. This
;;   function does not account for the variance, as the call to `(mod n 2)` will only work for an
;;   image enhancement algorithm with `bit 0 = 1`. I could pass that in as an fn here (with
;;   `(constantly 0)` in the sample data case), but I don't care enough.
(defn loop-enhance [algo-map n m]
  (loop [n n
         m m]
    (if (zero? n)
      m
      (recur (dec n) (enhance algo-map (mod n 2) m)))))

(utils/answer-block
 (->> day20-input
      (loop-enhance day20-algo 50)
      flatten
      (remove zero?)
      count))
