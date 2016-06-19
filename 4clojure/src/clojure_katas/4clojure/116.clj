(ns clojure-katas.4clojure.116
  "Prime Sandwich

  A <a href=\"http://en.wikipedia.org/wiki/Balanced_prime\">balanced prime</a>
  is a prime number which is also the mean of the primes directly before and
  after it in the sequence of valid primes.  Create a function which takes an
  integer n, and returns true iff it is a balanced prime.")

(def __
  (fn [n]
    (letfn [(prime? [n]
              (cond
                (< n 2) false
                (= n 2) true
                (= n 3) true
                (zero? (mod n 2)) false
                (zero? (mod n 3)) false
                :else (-> (for [i (range 6 (Math/sqrt n) 6)
                                :when (or (zero? (mod n (dec i)))
                                          (zero? (mod n (inc i))))]
                             i)
                          seq
                          nil?)))]
      (and (prime? n)
           (let [next-prime (first (drop-while (complement prime?) (range (+ n 1) Integer/MAX_VALUE)))
                 prev-prime (first (drop-while (complement prime?) (range (- n 1) 1 -1)))]
             (and prev-prime (= (/ (+ next-prime prev-prime) 2) n)))))))

(comment
  (= false (__ 4))
  (= true (__ 563))
  (= 1103 (nth (filter __ (range)) 15)))
