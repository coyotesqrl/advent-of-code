(ns coyotesqrl.2021.utils
  (:require [clojure.java.io :as io]))

(defn input->seq
  [filename]
  (->> (io/resource filename)
       io/reader
       line-seq))
