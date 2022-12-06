(ns day6
 (:require
  [clojure.set]))

(def input (slurp "day6.txt"))

(defn all-different? [xs]
  (= (count xs) (count (set xs))))

(defn solve [input n]
  (->> input
       (partition n 1)
       (take-while (complement all-different?))
       count
       (+ n)))

(comment
  (= 10 (solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4))
  (= 29 (solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14))
  (= 1361 (solve input)))
