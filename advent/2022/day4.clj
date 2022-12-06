(ns day4
 (:require
  [clojure.string :as s]))

(def input
  (let [raw    (slurp "day4.txt")
        s-expr (str "(" (s/replace (slurp "day4.txt") #"-" " ") ")")]
    (->> (read-string s-expr)
         (partition 4)
         (map (fn [[a1 a2 b1 b2]]
                [[a1 a2] [b1 b2]])))))

(defn fully-contains? [[a1 a2] [b1 b2]]
  (<= a1 b1 b2 a2))

(defn either-fully-contains? [a b]
  (or (fully-contains? a b)
      (fully-contains? b a)))

(defn overlaps? [a b]
  (let [[[a1 a2] [b1 b2]] (sort [a b])]
    (not (< a2 b1))))

(def result1
  (->> input
       (filter (partial apply either-fully-contains?))
       count))

(def result2
  (->> input
       (filter (partial apply overlaps?))
       count))

(comment
 (= 448 result1)
 (= 794 result2)
 input)
