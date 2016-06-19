(ns clojure-katas.algo)

(defn bfs [start goal? neighbors]
  (loop [queue [start]
         backtrack {start ::start}]
    (if-not (seq queue)
      nil
      (let [[node & remaining-queue] queue]
        (if (goal? node)
          (loop [path (list node)]
            (if (= ::start (backtrack (first path)))
              path
              (recur (conj path (backtrack (first path))))))
          (let [unseen (filter (complement backtrack) (neighbors node))]
            (recur (into remaining-queue unseen)
                   (into backtrack (map #(vec [% node]) unseen)))))))))

(defn gcd
  ([]
   1)
  ([a]
   a)
  ([a b]
   (if (= b 0)
     a
     (recur b (mod a b))))
  ([a b c & more]
   (reduce gcd (concat [a b c] more))))

