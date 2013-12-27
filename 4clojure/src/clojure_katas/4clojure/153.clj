(ns clojure-katas.4clojure.153
  "Pairwise Disjoint Sets

  Given a set of sets, create a function which returns true 
  if no two of
  those sets have any elements in common<sup>1</sup> and false otherwise.
  Some of the test cases are a bit tricky, so pay a little more attention to
  them.

  <sup>1</sup>Such sets are usually called pairwise disjoint or mutually
  disjoint.")

(def __
  (fn [s]
    (= (reduce + (map count s))
       (count (reduce clojure.set/union s)))))

(= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true)
(= (__ #{#{:a :b :c :d :e}
         #{:a :b :c :d}
         #{:a :b :c}
         #{:a :b}
         #{:a}})
   false)
(= (__ #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true)
(= (__ #{#{'a 'b}
         #{'c 'd 'e}
         #{'f 'g 'h 'i}
         #{''a ''c ''f}})
   true)
(= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
         #{#{:x :y :z} #{:x :y} #{:z} #{}}
         #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false)
(= (__ #{#{(= "true") false}
         #{:yes :no}
         #{(class 1) 0}
         #{(symbol "true") 'false}
         #{(keyword "yes") ::no}
         #{(class '1) (int \0)}})
   false)
(= (__ #{#{distinct?}
         #{#(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}})
   true)
(= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
         #{'+ '* mapcat (comment mapcat)}
         #{(do) set contains? nil?}
         #{, , , #_, , empty?}})
   false)
