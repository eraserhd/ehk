(ns clojure-katas.4clojure.92
  "Read Roman numerals

  Roman numerals are easy to recognize, but not everyone knows all the rules
  necessary to work with them. Write a function to parse a Roman-numeral
  string and return the number it represents.
  <br /><br />
  You can assume that
  the input will be well-formed, in upper-case, and follow the <a
  href=\"http://en.wikipedia.org/wiki/Roman_numerals#Subtractive_principle\">sub
  tractive principle</a>. You don't need to handle any numbers greater than
  MMMCMXCIX (3999), the largest number representable with ordinary letters.")

(def __
  (fn [number]
    (loop [total 0
           digits (reverse number)
           largest-digit-seen 0]
      (if-not (seq digits)
        total
        (let [digit-value ({\I 1
                            \V 5
                            \X 10
                            \L 50
                            \C 100
                            \D 500
                            \M 1000} (first digits))]
          (if (>= digit-value largest-digit-seen)
            (recur (+ total digit-value) (rest digits) digit-value)
            (recur (- total digit-value) (rest digits) largest-digit-seen)))))))

(= 14 (__ "XIV"))
(= 827 (__ "DCCCXXVII"))
(= 3999 (__ "MMMCMXCIX"))
(= 48 (__ "XLVIII"))
