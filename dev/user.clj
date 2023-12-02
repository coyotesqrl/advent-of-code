(ns user
  (:require [clojure.java.io :as io]
            [hato.client :as hc]
            [nextjournal.clerk :as clerk]
            [portal.api :as p]
            [selmer.parser :as sp]))

(defn portal []
  (add-tap #'portal.api/submit)
  (p/open {:launcher :intellij}))

(defn clerk
  ([] (clerk ["src/coyotesqrl/2023/"]))
  ([paths] (clerk/serve! {:browse? true :watch-paths paths :port 7778})))

(defn clerk-show [y d]
  (clerk/show! (format "src/coyotesqrl/%d/day%02d.clj" y d)))

(defn init-day [y d]
  (let [out-ns (format "src/coyotesqrl/%d/day%02d.clj" y d)
        input  (format "resources/coyotesqrl/%d/day%d-input.txt" y d)]
    (spit out-ns (-> "coyotesqrl/clj-template.txt"
                     (io/resource)
                     (sp/render-file {:year y :day d})))
    (spit input (-> "https://adventofcode.com/%d/day/%d/input"
                    (format y d)
                    (hc/get {:headers {"Cookie" (str "session=" (System/getenv "AOC_SESSION"))}})
                    :body))))
