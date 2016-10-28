(ns life-clj.core-test
  (:require [midje.sweet :refer :all]
            [life-clj.core :refer :all]))

(facts "about life"
  (+ 2 2) => 4)
