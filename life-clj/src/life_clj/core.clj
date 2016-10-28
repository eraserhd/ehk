(ns life-clj.core)

(defn board->set
  [board]
  (set (for [i (range (count board))
             :let [row (get board i)]
             j (range (count row))
             :when (not= \space (get row j))]
         [i j])))

(defn set->board
  [alive]
  (let [height (inc (transduce (map first) max 0 alive))
        width (inc (transduce (map second) max 0 alive))
        empty-row (vec (repeat width \space))
        empty-board (vec (repeat height empty-row))]
    (->> alive
      (reduce (fn [board [i j]]
                (assoc-in board [i j] \X))
              empty-board)
      (mapv (partial apply str)))))

(defn step
  [alive]
  (let [neighbors (for [[i j] alive
                        di [-1 0 +1]
                        dj [-1 0 +1]
                        :when (not= 0 di dj)]
                    [(+ i di) (+ j dj)])
        cell->neighbor-count (reduce
                               (fn [m [i j]]
                                 (update m [i j] (fnil inc 0)))
                               {}
                               neighbors)]
    (-> #{}
      (into
        (filter #(<= 1 (or (get cell->neighbor-count %) 0) 4))
        alive)
      (into 
        (comp
          (filter #(= 3 (second %)))
          (map first)
          (remove alive))
        cell->neighbor-count))))
