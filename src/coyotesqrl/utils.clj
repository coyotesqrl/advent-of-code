(ns coyotesqrl.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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
