(ns clojure-katas.4clojure-test
  (:require [clojure.test :refer :all]
            [clojure-katas.4clojure :refer :all]))

(def sample-problem
  {:restricted [],
   :title "Sequence of pronunciations",
   :times-solved 34,
   :difficulty "Medium",
   :scores
   {:101 0, :87 0, :117 1, :158 0, :159 1, :285 1, :83 0, :397 0, :82 0, :110 1,
    :275 1, :80 2, :112 1, :194 0, :127 1, :128 0, :77 0, :152 0, :79 0, :72 0,
    :71 3, :63 0, :124 1, :64 1, :218 1, :65 0, :89 1, :184 1, :181 1, :141 1,
    :136 2, :384 0, :95 1, :132 1, :96 1, :178 1, :134 0, :98 0, :149 2, :56 1,
    :250 1, :99 0, :170 0, :291 1, :130 0, :131 0, :104 0, :57 1, :84 1, :93 0,
    :85 0, :92 1, :86 1},
   :tests
   ["(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))"
    "(= [3 1 2 4] (first (__ [1 1 1 4 4])))"
    "(= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))"
    "(= 338 (count (nth (__ [3 2]) 15)))\r\n"],
   :user "mlni",
   :number 110,
   :description
   "<p>Write a function that returns a lazy sequence of \"pronunciations\" of a sequence of numbers. A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. For example, <code>[1 1]</code> is pronounced as <code>[2 1]</code> (\"two ones\"), which in turn is pronounced as <code>[1 2 1 1]</code> (\"one two, one one\").</p><p>Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, each element being a pronunciation of the previous element.</p>",
   :tags ["seqs"]})

(deftest problem-ns-test
  (is (= "(ns clojure-katas.4clojure.110\n  \"Sequence of pronunciations\")\n"
         (problem-ns sample-problem))))
