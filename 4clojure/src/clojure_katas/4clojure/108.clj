(ns clojure-katas.4clojure.108)

(def lazy-search

  (fn [& seqs]
    (loop [seqs seqs]
      (let [n (ffirst seqs)
            >=n-seqs (map (partial drop-while #(< % n)) seqs)]
        (if (apply = (map first >=n-seqs))
          n
          (recur (map (partial drop-while #(= n %)) >=n-seqs))))))

  )

(= 3 (lazy-search [3 4 5]))
(= 4 (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(= 7 (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13]))
(= 64 (lazy-search (map #(* % % %) (range)) ;; perfect cubes
                   (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                   (iterate inc 20))) ;; at least as large as 20
