(ns clojure-katas.4clojure.85
  "Power Set

  Write a function which generates the <a
  href=\"http://en.wikipedia.org/wiki/Power_set\">power set</a> of a given set.
  The power set of a set x is the set of all subsets of x, including the empty
  set and x itself.")

(def __
  (fn [s]
    (let [power-set (fn [f s]
                      (apply
                        clojure.set/union
                        #{s}
                        (for [e s]
                          (let [subset (disj s e)]
                            (f f subset)))))
          f (memoize power-set)]
      (f f s))))

(comment
  (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
  (= (__ #{}) #{#{}})
  (= (__ #{1 2 3})
     #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
  (= (count (__ (into #{} (range 10)))) 1024))
