(ns clojure-katas.4clojure.82)

(def word-chains

  (fn [ws]
    (let [edit-distance (fn [a b]
                          (let [dp (merge
                                     (apply hash-map
                                       (reduce into
                                               (for [i (range (+ 1 (count a)))]
                                                 [[i 0] i])))
                                     (apply hash-map
                                       (reduce into
                                               (for [j (range (+ 1 (count b)))]
                                                 [[0 j] j]))))]
                            (get
                              (reduce
                                (fn [dp [i j]]
                                  (assoc dp [i j] (min (+ 1 (dp [(- i 1) j]))
                                                       (+ 1 (dp [i (- j 1)]))
                                                       (+ (if (= (get a (- i 1)) (get b (- j 1)))
                                                            0
                                                            1)
                                                          (dp [(- i 1) (- j 1)])))))
                                dp
                                (for [i (range 1 (+ 1 (count a)))
                                      j (range 1 (+ 1 (count b)))]
                                  [i j]))
                              [(count a) (count b)])))

          connected? (fn [a b]
                       (= 1 (edit-distance a b)))

          edges (apply merge-with
                       clojure.set/union
                       (for [a ws
                             b ws
                             :when (and (not= a b) (connected? a b))]
                         {a #{b}}))]

      (letfn [(dfs [used at]
                (if (= used ws)
                  true
                  (some
                    #(dfs (conj used %) %)
                    (clojure.set/difference (get edges at) used))))]
        (or (some
              #(dfs #{%} %)
              ws)
            false))))

  )

(comment
  (= true (word-chains #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
  (= false (word-chains #{"cot" "hot" "bat" "fat"}))
  (= false (word-chains #{"to" "top" "stop" "tops" "toss"}))
  (= true (word-chains #{"spout" "do" "pot" "pout" "spot" "dot"}))
  (= true (word-chains #{"share" "hares" "shares" "hare" "are"}))
  (= false (word-chains #{"share" "hares" "hare" "are"})))
