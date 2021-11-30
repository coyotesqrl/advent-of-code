(ns build
  (:refer-clojure :exclude [test])
  (:require [org.corfield.build :as bb]))

(def lib 'com.dreamloom/advent-of-code)
(def version "0.1.0-SNAPSHOT")

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(defn ci "Run the CI pipeline of tests (and build the uberjar)." [opts]
      (-> opts
          (assoc :lib lib :version version)
          (bb/run-task [:outdated])
          (bb/run-task [:eastwood])
          (bb/run-task [:kondo])
          (bb/run-task [:fmt-check])
          (bb/run-tests)
          (bb/clean)
          (bb/uber)))
