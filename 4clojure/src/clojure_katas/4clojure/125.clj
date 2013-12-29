(ns clojure-katas.4clojure.125
  "Gus' Quinundrum

  Create a function of no arguments which returns a string that is an exact
  copy of the function itself.

  Hint: read <a
  href=\"http://en.wikipedia.org/wiki/Quine_(computing)\">this</a> if you get
  stuck (this question is harder than it first appears); but it's worth the
  effort to solve it independently if you can!

  Fun fact: Gus is
  the name of the <a href=\"http://i.imgur.com/FBd8z.png\">4Clojure dragon</a>.")

(def __ '(fn [] (let [a ["(fn [] (let [a " "] (str (first a) (pr-str a) (second a))))"]] (str (first a) (pr-str a) (second a)))))

(= (str __) ((eval __)))

