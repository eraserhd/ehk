(ns clojure-katas.4clojure.89)

(def can-make-tour?

  (fn [es]
    (let [vertice-edge-counts (loop [es (seq es)
                                     m {}]
                                (if-not es
                                  m
                                  (recur
                                    (next es)
                                    (let [[a b] (first es)]
                                      (merge-with + m {a 1} {b 1})))))

          n-odd (count (filter odd? (vals vertice-edge-counts)))

          edge-map (loop [es (seq es)
                          m {}]
                     (if-not es
                       m
                       (recur
                         (next es)
                         (let [[a b] (first es)]
                           (merge-with clojure.set/union
                                       m
                                       {a #{b}}
                                       {b #{a}})))))

          connected? (loop [q [(first (keys edge-map))]
                            seen #{(first (keys edge-map))}]
                       (if-not (seq q)
                         (= seen (into #{} (keys edge-map)))
                         (let [unseen (clojure.set/difference (get edge-map (first q)) seen)]
                           (recur
                             (into (rest q) unseen)
                             (into seen unseen)))))]

      (and (or (= n-odd 0)
               (= n-odd 2))
           connected?)))

  )


(= true (can-make-tour? [[:a :b]]))
(= false (can-make-tour? [[:a :a] [:b :b]]))
