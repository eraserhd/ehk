(ns clojure-katas.4clojure.119
  "Win at Tic-Tac-Toe

  As in <a href=\"/problem/73\">Problem 73</a>, a tic-tac-toe board is
  represented by a two dimensional vector. X is represented by :x, O is
  represented by :o, and empty is represented by :e. Create a function that
  accepts a game piece and board as arguments, and returns a set (possibly
  empty) of all valid board placements of the game piece which would result in
  an immediate win.

  Board coordinates should be as in calls to get-in. For example, [0 1] is
  the topmost row, center position.")

(def __

  (fn [me board]
    (let [positions [[[0 0] [0 1] [0 2]]
                     [[0 0] [1 0] [2 0]]
                     [[0 0] [1 1] [2 2]]
                     [[0 1] [1 1] [2 1]]
                     [[0 2] [1 2] [2 2]]
                     [[1 0] [1 1] [1 2]]
                     [[2 0] [2 1] [2 2]]
                     [[2 0] [1 1] [0 2]]]
          position-wins? (fn [[i j]]
                           (let [board (assoc-in board [i j] me)]
                             (seq
                               (filter
                                 identity
                                 (for [p positions]
                                   (apply = me (map #(get-in board %) p)))))))
          winning-boards (for [i (range 3)
                               j (range 3)
                               :when (= :e (get-in board [i j]))
                               :when (position-wins? [i j])]
                          [i j])]
      (into #{} winning-boards)))

  )

(comment
  (= (__ :x [[:o :e :e] 
             [:o :x :o] 
             [:x :x :e]])
     #{[2 2] [0 1] [0 2]})
  (= (__ :x [[:x :o :o] 
             [:x :x :e] 
             [:e :o :e]])
     #{[2 2] [1 2] [2 0]})
  (= (__ :x [[:x :e :x] 
             [:o :x :o] 
             [:e :o :e]])
     #{[2 2] [0 1] [2 0]})
  (= (__ :x [[:x :x :o] 
             [:e :e :e] 
             [:e :e :e]])
     #{})
  (= (__ :o [[:x :x :o] 
             [:o :e :o] 
             [:x :e :e]])
     #{[2 2] [1 1]}))
