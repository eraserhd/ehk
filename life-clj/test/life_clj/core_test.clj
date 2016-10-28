(ns life-clj.core-test
  (:require [midje.sweet :refer :all]
            [life-clj.core :refer :all]))

(def once (comp set->board step board->set))

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
  (fact "nothing spontaneously generates"
    (once [" "]) => [" "])
  (fact "lonely cells die"
    (once ["X"]) => [" "]
    (once ["XX"]) => [" "])
  (fact "overcrowded cells die"
    (once [" XXX "
           " XXX "
           " XXX "]) => ["  X  "
                         " X X "
                         "X   X"
                         " X X "
                         "  X  "])

  (-> ["XX" " X"] board->set step set->board) => ["XX" "XX"])
