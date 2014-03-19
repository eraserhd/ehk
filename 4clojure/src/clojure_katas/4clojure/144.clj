(ns clojure-katas.4clojure.144)

(def oscilrate

  (fn x [value f & fs]
    (lazy-seq
      (cons value (apply x (f value) (conj (vec fs) f)))))

  )

(comment
  (= (take 3 (oscilrate 3.14 int double)))
  (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
