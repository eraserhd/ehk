(ns day3
  (:require
   [clojure.java.io :as io]
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

(defn value [ch]
  (if (Character/isUpperCase ch)
    (+ (long ch) (- (long \A)) 27)
    (+ (long ch) (- (long \a)) 1)))

(assert (= 1 (value \a)))
(assert (= 2 (value \b)))
(assert (= 27 (value \A)))
(assert (= 28 (value \B)))

(defn solve []
  (->> (read-all "day3.txt")
       (map str)
       (map parse-rucksack)
       (map (fn [[a b]]
              (first (clojure.set/intersection a b))))
       (map value)
       (reduce +)))

(comment
 
  (= 8252 (solve))
  
  input
  (some? nil))

