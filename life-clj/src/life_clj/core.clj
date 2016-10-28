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
  (->> alive
    (reduce
       (fn [board [i j]]
         (assoc-in board [i j] \X))
       [[\space]])
    (mapv (partial apply str)))) 
