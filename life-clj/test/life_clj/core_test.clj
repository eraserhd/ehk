(ns life-clj.core-test
  (:require [midje.sweet :refer :all]
            [life-clj.core :refer :all]))

(facts "about board->set"
  (board->set [" "]) => #{}
  (board->set ["X"]) => #{[0 0]})
