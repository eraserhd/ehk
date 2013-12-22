(ns clojure-katas.4clojure.117
  "For Science!

  A mad scientist with tenure has created an experiment tracking mice in a
  maze.  Several mazes have been randomly generated, and you've been tasked
  with writing a program to determine the mazes in which it's possible for the
  mouse to reach the cheesy endpoint.  Write a function which accepts a maze
  in the form of a collection of rows, each row is a string
  where:

  * spaces represent areas where the mouse can walk freely
  * hashes (#) represent walls where the mouse can not walk
  * M represents the mouse's starting point
  * C represents the cheese which the mouse must reach

  The mouse is not allowed to travel diagonally in the maze (only
  up/down/left/right), nor can he escape the edge of the maze.  Your function
  must return true iff the maze is solvable by the mouse.")

(def __

  (fn [maze]
    (loop [queue (vec (for [i (range (count maze))
                            j (range (count (get maze i)))
                            :when (= \M (get-in maze [i j]))]
                        [i j]))
           seen #{(first queue)}]
      (if-not (seq queue)
        false
        (let [[[i j] & rest-of-queue] queue
              found? (= \C (get-in maze [i j]))
              unseen (for [[di dj] [[0 1] [0 -1] [1 0] [-1 0]]
                           :let [ii (+ i di)
                                 jj (+ j dj)]
                           :when (and (not (seen [ii jj]))
                                      (#{\space \C} (get-in maze [ii jj])))]
                       [ii jj])]
          (if found?
            true
            (recur (into rest-of-queue unseen)
                   (into seen unseen)))))))

  )

(= true  (__ ["M   C"]))
(= false (__ ["M # C"]))
(= true  (__ ["#######"
              "#     #"
              "#  #  #"
              "#M # C#"
              "#######"]))
(= false (__ ["########"
              "#M  #  #"
              "#   #  #"
              "# # #  #"
              "#   #  #"
              "#  #   #"
              "#  # # #"
              "#  #   #"
              "#  #  C#"
              "########"]))
(= false (__ ["M     "
              "      "
              "      "
              "      "
              "    ##"
              "    #C"]))
(= true  (__ ["C######"
              " #     "
              " #   # "
              " #   #M"
              "     # "]))
(= true  (__ ["C# # # #"
              "        "
              "# # # # "
              "        "
              " # # # #"
              "        "
              "# # # #M"]))
