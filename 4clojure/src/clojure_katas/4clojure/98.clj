(ns clojure-katas.4clojure.98
  "Equivalence Classes

  A function f defined on a domain D induces an <a
  href=\"http://en.wikipedia.org/wiki/Equivalence_relation\">equivalence
  relation</a> on D, as follows: a is equivalent to b with respect to f if and
  only if (f a) is equal to (f b).  Write a function with arguments f and D
  that computes the <a
  href=\"http://en.wikipedia.org/wiki/Equivalence_class\">equivalence
  classes</a> of D with respect to f.")

(def __
  (fn [f D]
    (->> D
         (map (fn [v] [(f v) #{v}]))
         (map #(apply hash-map %))
         (apply merge-with clojure.set/union)
         vals
         set)))

(comment
  (= (__ #(* % %) #{-2 -1 0 1 2})
     #{#{0} #{1 -1} #{2 -2}})
  (= (__ #(rem % 3) #{0 1 2 3 4 5 })
     #{#{0 3} #{1 4} #{2 5}})
  (= (__ identity #{0 1 2 3 4})
     #{#{0} #{1} #{2} #{3} #{4}})
  (= (__ (constantly true) #{0 1 2 3 4})
     #{#{0 1 2 3 4}}))
