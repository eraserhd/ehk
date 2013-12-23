(ns clojure-katas.4clojure.100
  "Least Common Multiple

  Write a function which calculates the <a
  href=\"http://en.wikipedia.org/wiki/Least_common_multiple\">least common
  multiple</a>.  Your function should accept a variable number of positive
  integers or ratios.")

(def __

  (fn [& args]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args)))

  )

(clojure-katas.algo/gcd 1/3 2/5)
(== (__ 2 3) 6)
(== (__ 5 3 7) 105)
(== (__ 1/3 2/5) 2)
(== (__ 3/4 1/6) 3/2)
(== (__ 7 5/7 2 3/5) 210)
