(ns clojure-katas.4clojure.84)

(def transitive-closure
  (fn [s]
    (letfn [(->map [s]
              (loop [s s
                     m {}]
                (let [[a b] (first s)]
                  (cond
                    (not (seq s)) m
                    (not (m a)) (recur (rest s) (conj m [a #{b}]))
                    :else (recur (rest s) (conj m [a (conj (m a) b)]))))))
            (compute [m]
              (into
                #{}
                (apply concat
                  (for [k (keys m)]
                    (let [values (loop [queue (vec (m k))
                                        seen (m k)]
                                   (cond
                                     (not (seq queue)) (vec seen)
                                     (not (seq (m (first queue)))) (recur (rest queue) seen)
                                     :else (let [unseen (filter (complement seen) (m (first queue)))]
                                             (recur
                                               (into (rest queue) unseen)
                                               (into seen unseen)))))]
                      (map #(vec [k %]) values))))))]
      (compute (->map s)))))

(let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (transitive-closure  divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
