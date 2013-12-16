(ns clojure-katas.4clojure.105)

(def __

  (fn [es]
    (loop [es es
           m {}]
      (if-not (seq es)
        m
        (let [k (first es)
              [vs next-es] (split-with (comp not keyword?) (rest es))]
          (recur
            next-es
            (conj m [k vs]))))))

  )

(= {} (__ []))
(= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))

