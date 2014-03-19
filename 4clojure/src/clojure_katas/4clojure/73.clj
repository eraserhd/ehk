(ns clojure-katas.4clojure.73)

(def analyze

  (fn [b]
    (let [possibilities [[[0 0] [0 1] [0 2]]
                         [[1 0] [1 1] [1 2]]
                         [[2 0] [2 1] [2 2]]
                         [[0 0] [1 0] [2 0]]
                         [[0 1] [1 1] [2 1]]
                         [[0 2] [1 2] [2 2]]
                         [[0 0] [1 1] [2 2]]
                         [[0 2] [1 1] [2 0]]]
          read-board (fn [possibility]
                       (for [[i j] possibility] ((b i) j)))
          winner (fn [p]
                   (and (apply = p)
                        (not= (first p) :e)))
          the-wins (filter winner (map read-board possibilities))]
      (if (seq the-wins)
        (ffirst the-wins)
        nil)))

  )

(comment
  (= nil (analyze [[:e :e :e]
                   [:e :e :e]
                   [:e :e :e]]))
  (= :x (analyze [[:x :e :o]
                  [:x :e :e]
                  [:x :e :o]])))
