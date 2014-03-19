(ns clojure-katas.4clojure.150
  "Palindromic Numbers

  A palindromic number is a number that is the same when written forwards or
  backwards (e.g., 3, 99, 14341).

  Write a function which takes an integer n, as its only argument, and
  returns an increasing lazy sequence of all palindromic numbers that are not
  less than n.

  The most simple solution will exceed the time limit!")

(def __
  (letfn [(digit-vector [n]
            (loop [n n
                   ds []]
              (if (zero? n)
                (if-not (seq ds)
                  [0]
                  ds)
                (recur (quot n 10) (conj ds (mod n 10))))))

          (digit-count [n]
            (count (digit-vector n)))

          (next-palindrome [n]
            (let [digits (digit-count n)
                  odd-digits? (odd? digits)
                  left-digits (quot (+ 1 digits) 2)
                  right-digits (- digits left-digits)
                  left (quot n (apply * (repeat right-digits 10)))
                  right (mod n (apply * (repeat right-digits 10)))

                  wrap? (not (= left-digits (digit-count (inc left))))

                  next-odd? (if wrap?
                              (not odd-digits?)
                              odd-digits?)
                  next-left-digits (cond
                                     (not wrap?) left-digits
                                     next-odd? (+ 1 left-digits)
                                     :else left-digits)
                  next-left (if wrap?
                              (apply * (repeat (dec next-left-digits) 10))
                              (inc left))
                  reconstruct (fn [left odd-digits?]
                                (loop [n left
                                       r (if odd-digits?
                                           (quot n 10)
                                           n)]
                                  (if (zero? r)
                                    n
                                    (recur (+' (*' n 10) (mod r 10))
                                           (quot r 10)))))]
              (if (> (reconstruct left odd-digits?) n)
                (reconstruct left odd-digits?)
                (reconstruct next-left next-odd?))))

          (palindrome? [n]
            (loop [ds (digit-vector n)]
              (if (>= 1 (count ds))
                true
                (if-not (= (first ds) (last ds))
                  false
                  (recur (subvec ds 1 (dec (count ds))))))))]
    (fn [n]
      (let [s (iterate next-palindrome n)]
        (if (palindrome? n)
          s
          (drop 1 s))))))

(comment
  (= (take 26 (__ 0))
     [0 1 2 3 4 5 6 7 8 9 
      11 22 33 44 55 66 77 88 99 
      101 111 121 131 141 151 161])
  (= (take 16 (__ 162))
     [171 181 191 202 
      212 222 232 242 
      252 262 272 282 
      292 303 313 323])
  (= (take 6 (__ 1234550000))
     [1234554321 1234664321 1234774321 
      1234884321 1234994321 1235005321])
  (= (first (__ (* 111111111 111111111)))
     (* 111111111 111111111))
  (= (set (take 199 (__ 0)))
     (set (map #(first (__ %)) (range 0 10000))))
  (= true 
     (apply < (take 6666 (__ 9999999))))
  (= (nth (__ 0) 10101)
     9102019))
