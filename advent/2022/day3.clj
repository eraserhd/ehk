(ns day3
  (:require
   [clojure.java.io :as io]
   [clojure.set]
   [clojure.string :as s]))

(defn read-all [path]
  (with-open [in (java.io.PushbackReader. (io/reader path))]
    (loop [xs []
           x (read in false ::eof)]
      (if (#{::eof} x)
        xs
        (recur (conj xs x) (read in false ::eof))))))

(defn parse-rucksack [s]
  (let [compartment-size (/ (count s) 2)
        compartment-a (set (subs s 0 compartment-size))
        compartment-b (set (subs s compartment-size))]
    [compartment-a compartment-b]))

(assert (= [#{\A \B \C} #{\D \E \F}] (parse-rucksack "ABCDEF")))

(defn priority [ch]
  (if (Character/isUpperCase ch)
    (+ (long ch) (- (long \A)) 27)
    (+ (long ch) (- (long \a)) 1)))

(defn common-item-priority [item-sets]
  (priority (first (apply clojure.set/intersection item-sets))))

(assert (= 1 (priority \a)))
(assert (= 2 (priority \b)))
(assert (= 27 (priority \A)))
(assert (= 28 (priority \B)))

(defn solve []
  (->> (read-all "day3.txt")
       (map str)
       (map parse-rucksack)
       (map common-item-priority)
       (reduce +)))

(defn solve2 []
  (->> (read-all "day3.txt")
       (map (comp set str))
       (partition 3)
       (map common-item-priority)
       (reduce +)))

(comment
 
  (= 8252 (solve))
  (= 2828 (solve2)))
