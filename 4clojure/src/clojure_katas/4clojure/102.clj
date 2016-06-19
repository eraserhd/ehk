(ns clojure-katas.4clojure.102
  "intoCamelCase

  When working with java, you often need to create an object with
  fieldsLikeThis, but you'd rather work with a hashmap that has
  :keys-like-this until it's time to convert. Write a function which takes
  lower-case hyphen-separated strings and converts them to camel-case strings.")

(def __

  (fn [s]
    (loop [s (vec s)
           r []
           dash? false]
      (cond
        (not (seq s)) (apply str r)
        (= \- (first s)) (recur (rest s) r true)
        dash? (recur (rest s) (conj r (Character/toUpperCase (first s))) false)
        :else (recur (rest s) (conj r (first s)) false))))

  )

(comment
  (= (__ "something") "something")
  (= (__ "multi-word-key") "multiWordKey")
  (= (__ "leaveMeAlone") "leaveMeAlone"))
