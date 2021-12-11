;; ## Advent of Code 2021
;; ### Day 9
(ns coyotesqrl.2021.day09
  (:require [coyotesqrl.utils :as utils]))

;; ##### Input
(defn- line->nums [l]
  (->> l
       (map str)
       (map #(Integer/parseInt %))))

(def day9-input (map line->nums (utils/input->seq "coyotesqrl/2021/day9-input.txt")))

(map line->nums '("2199943210" "3987894921" "9856789892" "8767896789" "9899965678"))

;; #### Part 1

(defn- row->min-map [r]
  (:mins (reduce-kv (fn [{:keys [prev mins] :as a} k v]
                      (assoc (if (or (nil? prev) (< v prev))
                               (assoc a :mins (-> mins
                                                  (dissoc (dec k))
                                                  (assoc k v)))
                               a)
                             :prev v))
                    {:prev nil :mins {}}
                    (vec r))))

(defn- input->min-rows [in]
  (->> in
       (map row->min-map)))

(defn- min-vertical? [n row col in]
  (let [col-neighbor (fn [f] (tap> {:row row :col col :f-row (f row)}) (nth (nth in (f row)) col))
        up           (if (zero? row)
                       9
                       (col-neighbor dec))
        down         (if (>= row (dec (count in)))
                       9
                       (col-neighbor inc))]
    (and (< n up) (< n down))))

(defn input->mins [in]
  (let [min-rows (vec (input->min-rows in))]
    (tap> {:min-rows min-rows})
    (reduce-kv (fn [row-a row-k row-v]
                 (into row-a (reduce (fn [col-a [col-k col-v]]
                                       (if (min-vertical? col-v row-k col-k in)
                                         (conj col-a col-v)
                                         col-a))
                                     []
                                     row-v)))
               []
               min-rows)))

(->> (input->mins day9-input)
     (map inc)
     (apply +))

;; ---
;; #### Part 2
;; Flood Filling
;;
;; *FAILURE*
;;
;; Look. So I know _what_ must be done. I just can't seem to successfully implement it. The recursive
;; solution is out of the question (thanks JVM, for still not having TCO) and I've tried,
;; unsuccessfully, several attempts at iterative solutions that all come up short. I did manage a
;; naive solution that was able to solve for the sample data because it doesn't have particularly
;; gnarly polygons. It's almost certainly convex polygons, where the convex vertices are to the
;; south and west that are giving me problems. I tried to account for that, thinking I could do
;; multiple runs, but then I started getting stuck in weird corners again.
;;
;; I even found a Clojure flood-filling library on GH that worked just fine...until I tried to run
;; that against the input data.
;;
;; I know this is probably not that difficult to solve...and yet.
;;
;; It's five hours away from the Day 11 drop; I haven't even looked at Day 10, and I'm mentally
;; drained. I think I'm taking a day or two away before I tackle D10, D11, and maybe even D12.
;;
;; Oh, and for giggles, should anyone have a 3d printer they can program, this could honestly be
;; solved more easily (for me) by literally flood filling:
;;
;; 1. Print the grid, with nothing where there's a `9`
;; 2. Take the resulting output and separate into the flooded basins
;; 3. Weigh; sort; take three heaviest
;;
;; The last step, multiplication, requires either being able to determine how many blocks are in
;; each of the segments or knowing what the weight for each grid is.
