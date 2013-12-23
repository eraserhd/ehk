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
  must return true iff the maze is solvable by the mouse."
  (:require [clojure-katas.algo :as algo]))

(def __

  (fn [maze]
    (not (nil? (algo/bfs
                 (first (for [i (range (count maze))
                              j (range (count (get maze i)))
                              :when (= \M (get-in maze [i j]))]
                          [i j]))
                 #(= \C (get-in maze %))
                 #(for [[di dj] [[0 1] [0 -1] [1 0] [-1 0]]
                        :let [i (+ di (first %))
                              j (+ dj (second %))]
                        :when (#{\space \C} (get-in maze [i j]))]
                    [i j])))))

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
