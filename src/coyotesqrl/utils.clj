(ns coyotesqrl.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

(defn input->str
  [filename]
  (->> (io/resource filename)
       io/reader
       slurp))

(defn input->seq
  [filename]
  (->> (io/resource filename)
       io/reader
       line-seq))

(defn input->one-line-numeric
  [filename]
  (mapv #(Long/parseLong %)
        (-> (input->seq filename)
            first
            (str/split #","))))

(defn extract-numbers [s]
  (->> (str/split s #"\s")
       (remove str/blank?)
       (map parse-long)))

(defn answer-block [ans]
  (clerk/html [:div {:style {:background-color :wheat
                             :text-align       :center
                             :border-color     :red
                             :border-width     1
                             :color           :black
                             :font-size        20}}
               ans]))
