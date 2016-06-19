(ns clojure-katas.4clojure.127
  "Love Triangle

  Everyone loves triangles, and it's easy to understand why–they're so
  wonderfully symmetric (except scalenes, they suck).

  Your passion for triangles has led you to become a miner (and part-time
  Clojure programmer) where you work all day to chip out isosceles-shaped
  minerals from rocks gathered in a nearby open-pit mine.  There are too many
  rocks coming from the mine to harvest them all so you've been tasked with
  writing a program to analyze the mineral patterns of each rock, and
  determine which rocks have the biggest minerals.

  Someone has already written a computer-vision system for the mine.  It
  images each rock as it comes into the processing centre and creates a cross-
  sectional bitmap of mineral (1) and rock (0) concentrations for each one.

  You must now create a function which accepts a collection of integers, each
  integer when read in base-2 gives the bit-representation of the rock (again,
  1s are mineral and 0s are worthless scalene-like rock).  You must return the
  cross-sectional area of the largest harvestable mineral from the input rock,
  as follows:

  * The minerals only have smooth faces when sheared vertically or
    horizontally from the rock's cross-section
  * The mine is only concerned with harvesting isosceles triangles (such that
    one or two sides can be sheared)
  * If only one face of the mineral is sheared, its opposing vertex must be a
    point (ie. the smooth face must be of odd length), and its two equal-
    length sides must intersect the shear face at 45º (ie. those sides must
    cut even-diagonally)
  * The harvested mineral may not contain any traces of rock
  * The mineral may lie in any orientation in the plane
  * Area should be calculated as the sum of 1s that comprise the mineral
  * Minerals must have a minimum of three measures of area to be harvested
  * If no minerals can be harvested from the rock, your function should return
    nil")

(def __
  (fn [input]
    (let [compile-pattern (fn [p]
                            (reduce (fn [x n] (bit-or (bit-shift-left x 5) n)) 0 (reverse p)))
          input (compile-pattern input)

          basic-patterns (map compile-pattern
                              [[2r11111
                                2r01111
                                2r00111
                                2r00011
                                2r00001]
                               [2r01111
                                2r00111
                                2r00011
                                2r00001]
                               [2r00111
                                2r00011
                                2r00001]
                               [2r00011
                                2r00001]

                               [2r11111
                                2r01110
                                2r00100]
                               [2r00111
                                2r00010]

                               [2r00001
                                2r00011
                                2r00001]
                               [2r00001
                                2r00011
                                2r00111
                                2r00011
                                2r00001]])

          column-pattern (fn [n]
                           (compile-pattern (repeat 6 (bit-shift-left 1 n))))
          row-pattern (fn [n]
                        (compile-pattern (concat (repeat n 2r00000) [2r11111])))

          left-edge (column-pattern 4)
          bottom-edge (row-pattern 5)

          clear? (fn [p x] (zero? (bit-and p x)))
          matches? (fn [x] (= x (bit-and x input)))

          left-shifts (fn [p]
                        (loop [p p
                               shifts [p]]
                          (if (clear? left-edge p)
                            (recur (bit-shift-left p 1)
                                   (conj shifts (bit-shift-left p 1)))
                            shifts)))

          down-shifts (fn [p]
                        (loop [p p
                               shifts [p]]
                          (if (clear? bottom-edge p)
                            (recur (bit-shift-left p 5)
                                   (conj shifts (bit-shift-left p 5)))
                            shifts)))

          flip-horizontally (fn [p]
                              (bit-or
                                (bit-shift-left (bit-and p (column-pattern 0)) 4)
                                (bit-shift-left (bit-and p (column-pattern 1)) 2)
                                (bit-and p (column-pattern 2))
                                (bit-shift-right (bit-and p (column-pattern 3)) 2)
                                (bit-shift-right (bit-and p (column-pattern 4)) 4)))

          flip-vertically (fn [p]
                            (bit-or
                              (bit-shift-left (bit-and p (row-pattern 0)) (* 5 5))
                              (bit-shift-left (bit-and p (row-pattern 1)) (* 5 3))
                              (bit-shift-left (bit-and p (row-pattern 2)) (* 5 1))
                              (bit-shift-right (bit-and p (row-pattern 3)) (* 5 1))
                              (bit-shift-right (bit-and p (row-pattern 4)) (* 5 3))
                              (bit-shift-right (bit-and p (row-pattern 5)) (* 5 5))))

          all-patterns (->> basic-patterns
                         (map left-shifts)
                         flatten
                         (map down-shifts)
                         flatten
                         (map (fn [p] [p (flip-horizontally p)]))
                         flatten
                         (map (fn [p] [p (flip-vertically p)]))
                         flatten
                         (into #{}))

          matching (->> all-patterns
                     (filter matches?)
                     (map #(Long/bitCount %)))]

      (if-not (empty? matching)
        (apply max matching)))))

(comment
  (= 10 (__ [15 15 15 15 15]))
; 1111      1111
; 1111      *111
; 1111  ->  **11
; 1111      ***1
; 1111      ****
  (= 15 (__ [1 3 7 15 31]))
; 00001      0000*
; 00011      000**
; 00111  ->  00***
; 01111      0****
; 11111      *****
  (= 3 (__ [3 3]))
; 11      *1
; 11  ->  **
  (= 4 (__ [7 3]))
; 111      ***
; 011  ->  0*1
  (= 6 (__ [17 22 6 14 22]))
; 10001      10001
; 10110      101*0
; 00110  ->  00**0
; 01110      0***0
; 10110      10110
  (= 9 (__ [18 7 14 14 6 3]))
; 10010      10010
; 00111      001*0
; 01110      01**0
; 01110  ->  0***0
; 00110      00**0
; 00011      000*1
  (= nil (__ [21 10 21 10]))
; 10101      10101
; 01010      01010
; 10101  ->  10101
; 01010      01010
  (= nil (__ [0 31 0 31 0]))
; 00000      00000
; 11111      11111
; 00000  ->  00000
; 11111      11111
; 00000      00000  )
