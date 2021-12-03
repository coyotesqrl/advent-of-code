(ns user
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [portal.api :as p]))

(defn portal
  ([] (portal nil))
  ([l]
   (do (add-tap #'portal.api/submit)
       (p/open {:launcher l}))))

(defn clerk
  ([] (clerk ["src/coyotesqrl/2021/"]))
  ([paths] (clerk/serve! {:browse? true :watch-paths paths})))

(defn clerk-show [y d]
  (clerk/show! (format "src/coyotesqrl/%d/day%d.clj" y d)))

(defn clerk-static [y]
  (let [paths (->> y
                   (format "src/coyotesqrl/%d")
                   (io/file)
                   file-seq
                   (filter #(.isFile %))
                   (map #(.getPath %)))]
    (prn (vector paths))
    (clerk/build-static-app! {:paths paths
                              :out-path (format "doc/%d" y y)})))

