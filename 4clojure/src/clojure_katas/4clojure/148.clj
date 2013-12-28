(ns clojure-katas.4clojure.148
  "The Big Divide

  Write a function which calculates the sum of all natural numbers under n
  (first argument) which are evenly divisible by at least one of a and b
  (second and third argument). Numbers a and b are guaranteed to be <a
  href=\"http://en.wikipedia.org/wiki/Coprime\">coprimes</a>.

  Note: Some test cases have a very large n, so the most obvious solution
  will exceed the time limit.")

(def __
  (fn [n a b]
    (letfn [(f [x]
              (let [count (quot (dec' n) x)]
                (-> (*' count (inc' count))
                    (/ 2)
                    (*' x))))]
      (+' (f a)
          (f b)
          (-' (f (*' a b)))))))

(= 0 (__ 3 17 11))
(= 23 (__ 10 3 5))
(= 233168 (__ 1000 3 5))
(= "2333333316666668" (str (__ 100000000 3 5)))
(= "110389610389889610389610"
  (str (__ (* 10000 10000 10000) 7 11)))
(= "1277732511922987429116"
  (str (__ (* 10000 10000 10000) 757 809)))
(= "4530161696788274281"
  (str (__ (* 10000 10000 1000) 1597 3571)))
