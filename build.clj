(ns build
  (:refer-clojure :exclude [test])
  (:require [org.corfield.build :as bb]))

(def lib 'com.dreamloom/advent-of-code)
(def version "0.1.0-SNAPSHOT")

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(defn static "Run the static checks of the project." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/run-task [:outdated])
      #_(bb/run-task [:eastwood])
      (bb/run-task [:kondo])
      (bb/run-task [:fmt-check])))

(defn fmt-fix
  "Fix code formatted incorrectly"
  [opts]
  (-> opts
      (bb/run-task [:fmt-fix])))

(defn ci "Run the static tools, tests, and generate static site pages." [opts]
  (-> opts
      static
      #_(bb/run-tests)
      (bb/run-task [:static-gen])))
