(ns clojure-katas.4clojure.94
  "Game of Life

  The <a href=\"http://en.wikipedia.org/wiki/Conway's_Game_of_Life\">game of
  life</a> is a cellular automaton devised by mathematician John Conway. 

  The 'board' consists of both live (#) and dead ( ) cells. Each cell
  interacts with its eight neighbours (horizontal, vertical, diagonal), and
  its next state is dependent on the following rules:

  1) Any live cell with fewer than two live neighbours dies, as if caused by
     under-population.
  2) Any live cell with two or three live neighbours lives on to the next
     generation.
  3) Any live cell with more than three live neighbours dies, as if by 
     overcrowding.
  4) Any dead cell with exactly three live neighbours becomes a live cell, as 
     if by reproduction.

  Write a function that accepts a board, and returns a board representing the
  next generation of cells.")

(def __
  (fn [starting-board]
    (let [living? (fn [[i j]]
                    (= \# (get-in starting-board [i j])))
          neighbors (fn [[i j]]
                      (count
                        (filter identity
                                (for [[id jd] [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]]]
                                  (living? [(+ i id) (+ j jd)])))))
          should-live? (fn [p]
                         (case (neighbors p)
                           0 false
                           1 false
                           2 (living? p)
                           3 true
                           4 false
                           5 false
                           6 false
                           7 false
                           8 false))]
      (for [i (range (count starting-board))]
        (apply str
               (for [j (range (count (get starting-board i)))]
                 (if (should-live? [i j])
                   \#
                   \space)))))))

(= (__ ["      "  
        " ##   "
        " ##   "
        "   ## "
        "   ## "
        "      "])
   ["      "  
    " ##   "
    " #    "
    "    # "
    "   ## "
    "      "])
(= (__ ["     "
        "     "
        " ### "
        "     "
        "     "])
   ["     "
    "  #  "
    "  #  "
    "  #  "
    "     "])
(= (__ ["      "
        "      "
        "  ### "
        " ###  "
        "      "
        "      "])
   ["      "
    "   #  "
    " #  # "
    " #  # "
    "  #   "
    "      "])
