(ns life-clj.core-test
  (:require [midje.sweet :refer :all]
            [life-clj.core :refer :all]))

(facts "about board->set"
  (board->set [" "]) => #{}
  (board->set ["X"]) => #{[0 0]}
  (board->set ["X X"
               " XX"
               "X  "]) => #{[0 0] [0 2] [1 1] [1 2] [2 0]})

(facts "about set->board"
  (set->board #{}) => [" "]
  (set->board #{[0 0]}) => ["X"]
  (set->board #{[0 0] [1 1]}) => ["X " " X"])

(facts "about simulating life"
  (-> [" "] board->set step set->board) => [" "])
