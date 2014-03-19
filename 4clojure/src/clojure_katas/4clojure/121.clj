(ns clojure-katas.4clojure.121
  "Universal Computation Engine

  Given a mathematical formula in prefix notation, return a function that
  calculates the value of the formula.
  	 The formula can contain nested
  calculations using the four basic
  	 mathematical operators, numeric
  constants, and symbols representing variables.
  	 The returned function has
  to accept a single parameter containing the map
  	 of variable names to their
  values.")

(def __

  (fn f [e]
    (fn [m]
      (if (coll? e)
        (apply ({'+ + '- - '* * '/ /} (first e)) (map #((f %) m) (rest e)))
        (or (m e) e))))

  )

(comment
  (= 2 ((__ '(/ a b))
        '{b 8 a 16}))
  (= 8 ((__ '(+ a b 2))
        '{a 2 b 4}))
  (= [6 0 -4]
     (map (__ '(* (+ 2 a)
                  (- 10 b)))
          '[{a 1 b 8}
            {b 5 a -2}
            {a 2 b 11}]))
  (= 1 ((__ '(/ (+ x 2)
                (* 3 (+ y 1))))
        '{x 4 y 1})))
