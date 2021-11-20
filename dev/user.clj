(ns user
  (:require [nextjournal.clerk :as clerk]
            [portal.api :as p]))

(defn portal
  ([] portal nil)
  ([l]
   (do (add-tap #'portal.api/submit)
       (p/open {:launcher l}))))

(defn clerk
  ([] (clerk ["src"]))
  ([paths] (clerk/serve! {:browse? true :watch-paths paths})))
