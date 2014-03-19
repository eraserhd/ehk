(ns clojure-katas.4clojure.113)

(def __

  (fn [& xs]
    (reify
      Object
      (toString [this] (clojure.string/join ", " (sort xs)))
      clojure.lang.Seqable
      (seq [this] (not-empty (distinct xs)))))

  )

(comment
  (= "1, 2, 3" (str (__ 2 1 3)))
  (= '(2 1 3) (seq (__ 2 1 3)))
  (= '(2 1 3) (seq (__ 2 1 3 3 1 2)))
  (= '(1) (seq (apply __ (repeat 5 1))))
  (= "1, 1, 1, 1, 1" (str (apply __ (repeat 5 1))))
  (and (= nil (seq (__)))
       (=  "" (str (__)))))
