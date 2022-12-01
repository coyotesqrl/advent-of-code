(ns static-gen
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]))

(defn- generate-docs [year]
  (let [paths (->> year
                   (format "src/coyotesqrl/%d")
                   (io/file)
                   file-seq
                   (filter #(.isFile %))
                   (map #(.getPath %)))]
    (clerk/build! {:paths    paths
                   :out-path (format "docs/%d" year)})))

(defn -main [& years]
  (try (doseq [year years]
         (generate-docs (Integer/parseInt year)))
       (System/exit 0)
       (catch Throwable t
         (prn t)
         (System/exit 1))))
