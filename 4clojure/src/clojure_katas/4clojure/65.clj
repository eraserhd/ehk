(ns clojure-katas.4clojure.65)

(def black-box-testing

  (fn [s]
    (cond
      (= :map-value (:map-key (conj s [:map-key :map-value]))) :map
      (= :list-head (first (conj (conj s :aaa) :list-head))) :list
      (let [s2 (conj (conj s :set-entry) :set-entry)]
        (= (count s2) (+ 1 (count s)))) :set
      :else :vector))

  )

(= :map (black-box-testing {:a 1, :b 2}))
(= :list (black-box-testing (range (rand-int 20))))
(= :vector (black-box-testing [1 2 3 4 5 6]))
(= :set (black-box-testing #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))
