(ns clojure-katas.4clojure.158
  "Decurry

  Write a function that accepts a curried function of unknown arity n.  Return
  an equivalent function of n arguments.


  You may wish to read <a
  href=\"http://en.wikipedia.org/wiki/Currying\">this</a>.")

(def __

  (fn [f]
    (fn [& args]
      (reduce #(%1 %2) f args)))

  )

(comment
  (= 10 ((__ (fn [a]
               (fn [b]
                 (fn [c]
                   (fn [d]
                     (+ a b c d))))))
         1 2 3 4))
  (= 24 ((__ (fn [a]
               (fn [b]
                 (fn [c]
                   (fn [d]
                     (* a b c d))))))
         1 2 3 4))
  (= 25 ((__ (fn [a]
               (fn [b]
                 (* a b))))
         5 5)))
