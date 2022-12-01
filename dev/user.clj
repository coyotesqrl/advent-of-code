(ns user
  (:require [nextjournal.clerk :as clerk]
            [portal.api :as p]))

(defn portal []
  (add-tap #'portal.api/submit)
  (p/open {:launcher :intellij}))

(defn clerk
  ([] (clerk ["src/coyotesqrl/2022/"]))
  ([paths] (clerk/serve! {:browse? true :watch-paths paths})))

(defn clerk-show [y d]
  (clerk/show! (format "src/coyotesqrl/%d/day%02d.clj" y d)))
