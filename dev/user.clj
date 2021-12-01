(ns user
  (:require [nextjournal.clerk :as clerk]
            [portal.api :as p]))

(defn portal
  ([] (portal nil))
  ([l]
   (do (add-tap #'portal.api/submit)
       (p/open {:launcher l}))))

(defn clerk
  ([] (clerk ["src/coyotesqrl/2021/core.clj"]))
  ([paths] (clerk/serve! {:browse? true :watch-paths paths})))

(defn show [f]
  (clerk/show! f))
