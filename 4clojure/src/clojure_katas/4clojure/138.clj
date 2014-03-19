(ns clojure-katas.4clojure.138
  "Squares Squared

  Create a function of two integer arguments: the start and end, respectively.
  You must create a vector of strings which renders a 45&deg; rotated square
  of integers which are successive squares from the start point up to and
  including the end point.  If a number comprises multiple digits, wrap them
  around the shape individually.  If there are not enough digits to complete
  the shape, fill in the rest with asterisk characters.  The direction of the
  drawing should be clockwise, starting from the center of the shape and
  working outwards, with the initial direction being down and to the right.")

(def __
  (fn [start end]
    (let [squares (take-while (partial >= end) (iterate #(* % %) start))
          digits (apply str (map str squares))
          directions (cycle [[1 1] [1 -1] [-1 -1] [-1 1]])
          segment-lengths (map #(inc (quot % 2)) (range))
          movements (mapcat repeat segment-lengths directions)
          starting-points [[0 0] [-1 0]]
          position-seqs (for [starting-point starting-points]
                          (take (count digits)
                                (reductions (partial map +)
                                            starting-point
                                            movements)))
          abs #(if (< % 0) (- %) %)
          bound #(reduce (fn [a [i j]] (max a (abs i) (abs j))) 0 %)
          bounds (map bound position-seqs)

          better-fit (if (< (nth bounds 0) (nth bounds 1))
                       0
                       1)

          starting-point (nth starting-points better-fit)
          size (nth bounds better-fit)
          positions (nth position-seqs better-fit)

          width (inc (* 2 size))

          empty-board (vec (repeat width (vec (repeat width \space))))
          board-with-digits (reduce
                              #(assoc-in %1 (map (partial + size) (first %2)) (second %2))
                              empty-board
                              (map vector positions digits))
          board-with-stars (reduce
                             (fn [b [i j]]
                               (if (= \space (get-in b [i j]))
                                 (assoc-in b [i j] \*)
                                 b))
                             board-with-digits
                             (take-while
                               #(get-in empty-board %)
                               (reductions (partial map +)
                                           (map (partial + size) starting-point)
                                           movements)))]
      (map (partial apply str) board-with-stars))))

(comment
  (= (__ 2 2) ["2"])
  (= (__ 2 4) [" 2 "
               "* 4"
               " * "])
  (= (__ 3 81) [" 3 "
                "1 9"
                " 8 "])
  (= (__ 4 20) [" 4 "
                "* 1"
                " 6 "])
  (= (__ 2 256) ["  6  "
                 " 5 * "
                 "2 2 *"
                 " 6 4 "
                 "  1  "])
  (= (__ 10 10000) ["   0   "
                    "  1 0  "
                    " 0 1 0 "
                    "* 0 0 0"
                    " * 1 * "
                    "  * *  "
                    "   *   "]))
