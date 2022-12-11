(ns day10
 (:require
  [clojure.string :as s]))

(def example-input "noop
addx 3
addx -5")

(def larger-input "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")
(def input (slurp "day10.txt"))

(defn parse [input]
  (read-string (str "(" input ")")))

(defn solve [input]
  (->> (parse input)
       (map #(if (number? %) % 0))
       (reductions + 1)
       (drop 19)
       (partition 1 40)
       flatten
       (map * (iterate (partial + 40) 20))
       (reduce +)))

(= 13140 (solve larger-input))
(= 17940 (solve input))

(defn solve2 [input]
  (->> (parse input)
       (map #(if (number? %) % 0))
       (reductions + 1)
       (map (fn [clock x]
              (if (<= (dec x) (mod clock 40) (inc x))
                \#
                \.))
            (iterate inc 0))
       (partition 40)
       (map (partial apply str))
       (s/join "\n")))

(= (solve2 larger-input)
   (str "##..##..##..##..##..##..##..##..##..##..\n"
        "###...###...###...###...###...###...###.\n"
        "####....####....####....####....####....\n"
        "#####.....#####.....#####.....#####.....\n"
        "######......######......######......####\n"
        "#######.......#######.......#######....."))
(= (solve2 input)
   (str "####..##..###...##....##.####...##.####.\n"
        "...#.#..#.#..#.#..#....#.#.......#....#.\n"
        "..#..#....###..#..#....#.###.....#...#..\n"
        ".#...#....#..#.####....#.#.......#..#...\n"
        "#....#..#.#..#.#..#.#..#.#....#..#.#....\n"
        "####..##..###..#..#..##..#.....##..####."))
