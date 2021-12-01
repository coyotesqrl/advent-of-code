(ns coyotesqrl.utils
  (:require [clojure.java.io :as io]))

(defn input->seq
  [filename]
  (->> (io/resource filename)
       io/reader
       line-seq))
