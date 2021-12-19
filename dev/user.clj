(ns user
  (:require [nextjournal.clerk :as clerk]
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
  (clerk/show! (format "src/coyotesqrl/%d/day%02d.clj" y d)))
