(ns clojure-katas.4clojure.131
  "Sum Some Set Subsets

  Given a variable number of sets of integers, create a function which returns
  true iff all of the sets have a non-empty subset with an equivalent
  summation.")

(def __
  (fn [& sets]
    (let [sums (fn sums
                 ([s]
                  (reduce
                    into
                    #{}
                    (for [e s]
                      (sums e (disj s e)))))
                 ([n s]
                  (if-not (seq s)
                    #{n}
                    (clojure.set/union
                      (sums (+ n (first s)) (rest s))
                      (sums n (rest s))))))]
      (->> sets
           (map sums)
           (apply clojure.set/intersection)
           empty?
           not))))

(= true  (__ #{-1 1 99} 
             #{-2 2 888}
             #{-3 3 7777})) ; ex. all sets have a subset which sums to zero
(= false (__ #{1}
             #{2}
             #{3}
             #{4}))
(= true  (__ #{1}))
(= false (__ #{1 -3 51 9} 
             #{0} 
             #{9 2 81 33}))
(= true  (__ #{1 3 5}
             #{9 11 4}
             #{-3 12 3}
             #{-3 4 -2 10}))
(= false (__ #{-1 -2 -3 -4 -5 -6}
             #{1 2 3 4 5 6 7 8 9}))
(= true  (__ #{1 3 5 7}
             #{2 4 6 8}))
(= true  (__ #{-1 3 -5 7 -9 11 -13 15}
             #{1 -3 5 -7 9 -11 13 -15}
             #{1 -1 2 -2 4 -4 8 -8}))
(= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
             #{10 -9 8 -7 6 -5 4 -3 2 -1}))
