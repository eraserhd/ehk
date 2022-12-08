(ns user
 (:require
  [clojure.repl :refer :all]
  [nextjournal.clerk :as clerk]
  [nextjournal.clerk.viewer :as viewer]))

(clerk/serve! {:browse? true, :watch-paths ["."]})
