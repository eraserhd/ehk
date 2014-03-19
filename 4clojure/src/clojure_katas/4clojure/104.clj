(ns clojure-katas.4clojure.104
  "Write Roman Numerals

  This is the inverse of <a href='92'>Problem 92</a>, but much easier. Given
  an integer smaller than 4000, return the corresponding roman numeral in
  uppercase, adhering to the <a
  href='http://www.numericana.com/answer/roman.htm#valid'>subtractive
  principle</a>.")

(def __
  (fn [n]
    (letfn [(digit [n d1 d5 d10]
              (apply
                str
                (case (mod n 10)
                  0 ""
                  1 [d1]
                  2 [d1 d1]
                  3 [d1 d1 d1]
                  4 [d1 d5]
                  5 [d5]
                  6 [d5 d1]
                  7 [d5 d1 d1]
                  8 [d5 d1 d1 d1]
                  9 [d1 d10])))]
      (str
        (case (quot n 1000)
          0 ""
          1 "M"
          2 "MM"
          3 "MMM"
          4 "MMMM")
        (digit (quot n 100) \C \D \M)
        (digit (quot n 10) \X \L \C)
        (digit n \I \V \X)))))

(comment
  (= "I" (__ 1))
  (= "XXX" (__ 30))
  (= "IV" (__ 4))
  (= "CXL" (__ 140))
  (= "DCCCXXVII" (__ 827))
  (= "MMMCMXCIX" (__ 3999))
  (= "XLVIII" (__ 48)))
