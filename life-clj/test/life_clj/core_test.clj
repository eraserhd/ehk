(ns life-clj.core-test
  (:require [midje.sweet :refer :all]
            [life-clj.core :refer :all]))

(def once (comp set->board step board->set))

(defmacro example
  [title & args]
  (let [p (->> args
           (filter string?)
           (partition 2))
        left (vec (map first p))
        right (vec (map second p))]
    `(fact ~title (once ~left) => ~right)))

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
  (example "nothing spontaneously generates"
    " "  ==> " ")
  (facts "about lonely cells"
    (example "cell with no neighbors dies"
      "X"  ==> " ")
    (example "cell with single neighbor dies"
      "XX" ==> " "))
  (facts "about stable cells"
    (example "cell with two neighbors lives"
      "   "     "X"
      "XXX" ==> "X"
      "   "     "X") 
    (example "cell with three neighbors lives"
      " X "     "XXX"
      "XXX" ==> "XXX"
      "   "     " X "))
  (facts "about overcrowding cells dying"
    (example "4 neighbors"
      " X "     "XXX"
      "XXX" ==> "X X"
      " X "     "XXX")
    (example "5 and 8 neighbors"
      "     "     "  X  "
      " XXX "     " X X "
      " XXX " ==> "X   X"
      " XXX "     " X X "
      "     "     "  X  ")
    (example "6 neighbors"
      "    "     " X  "
      "XXX " ==> "X X "
      " XX "     "   X"
      " XX "     " XX ")
    (example "7 neighbors"
      "    "     " X  "
      "XXX "     "X X "
      " XX " ==> "   X"
      "XXX "     "X X "
      "    "     " X  "))
  (example "empty cell with three neighbors births"
    "XX" ==> "XX"
    " X"     "XX"))
