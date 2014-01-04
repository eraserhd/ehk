(ns clojure-katas.4clojure.124
  "Analyze Reversi

  <a href=\"http://en.wikipedia.org/wiki/Reversi\">Reversi</a> is normally
  played on an 8 by 8 board. In this problem, a 4 by 4 board is represented as
  a two-dimensional vector with black, white, and empty pieces represented by
  'b, 'w, and 'e, respectively. Create a function that accepts a game board
  and color as arguments, and returns a map of legal moves for that color.
  Each key should be the coordinates of a legal move, and its value a set of
  the coordinates of the pieces flipped by that move.

  Board coordinates should be as in calls to get-in. For example, [0 1] is
  the topmost row, second column from the left.")

(def __
  (fn [board me]
    (let [not-me (if (= 'w me) 'b 'w)
          directions (for [i (range -1 2)
                           j (range -1 2)
                           :when (not= [0 0] [i j])]
                       [i j])
          flips-in-direction (fn [from direction]
                               (let [spaces (->> (iterate #(map + % direction) from)
                                                 (drop 1)
                                                 (take-while #(if-let [piece (get-in board %)]
                                                                (not= 'e piece))))
                                     pieces (map #(get-in board %) spaces)]
                                 (if (and (= not-me (first pieces)) (some #{me} pieces))
                                   (set (take (count (take-while #(= not-me %) pieces)) spaces))
                                   #{})))
          flips (fn [from]
                  (reduce
                    clojure.set/union
                    (for [direction directions]
                      (flips-in-direction from direction))))
          can-move? (fn [position]
                      (and
                        (= 'e (get-in board position))
                        (not (zero? (count (flips position))))))
          valid-moves (for [i (range (count board))
                            j (range (count (get board i)))
                            :when (can-move? [i j])]
                        [i j])]
      (into {} (mapcat #(vector [% (flips %)]) valid-moves)))))

(= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
   (__ '[[e e e e]
         [e w b e]
         [e b w e]
         [e e e e]] 'w))
(= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
   (__ '[[e e e e]
         [e w b e]
         [w w w e]
         [e e e e]] 'b))
(= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
   (__ '[[e e e e]
         [e w b e]
         [w w b e]
         [e e b e]] 'w))
(= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
   (__ '[[e e w e]
         [b b w e]
         [b w w e]
         [b w w w]] 'b))
