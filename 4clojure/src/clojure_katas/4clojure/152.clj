(ns clojure-katas.4clojure.152
  "Latin Square Slicing

  A <a href=\"http://en.wikipedia.org/wiki/Latin_square\">Latin square</a> of
  order n is an n x n array that contains n different elements, 
  each occurring exactly once in each row, and exactly once in each column. 
  For example, among the following arrays only the first one forms a Latin
  square:
    A B C    A B C    A B C
    B C A    B C A    B D A
    C A B    C A C
    C A B

  Let V be a vector of such vectors that they may differ in length.  We will
  say that an arrangement of vectors of V in consecutive rows is an 
  alignment (of vectors) of V if the following conditions are satisfied:
  
  * All vectors of V are used.
  * Each row contains just one vector.
  * The order of V is preserved.
  * All vectors of maximal length are horizontally aligned each other.
  * If a vector is not of maximal length then all its elements are aligned with
    elements of some <a href=\"http://clojuredocs.org/clojure_core/clojure.core/subvec\">subvector</a>
    of a vector of maximal length.</li>

  Let L denote a Latin square of order 2 or greater.  We will say that L
  _is included_ in V or that V _includes_ L iff there exists an alignment of V
  such that contains a subsquare that is equal to L.

  For example, if V equals [[1 2 3][2 3 1 2 1][3 1 2]] 
  then there are nine alignments of V (brackets omitted):
  
          1              2            3

        1 2 3          1 2 3          1 2 3
    A   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
        3 1 2        3 1 2        3 1 2

        1 2 3          1 2 3          1 2 3
    B   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
          3 1 2        3 1 2        3 1 2

        1 2 3          1 2 3          1 2 3
    C   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
            3 1 2        3 1 2        3 1 2
  
  Alignment A1 contains Latin square [[1 2 3][2 3 1][3 1 2]], alignments A2,
  A3, B1, B2, B3 contain no Latin squares, and alignments C1, C2, C3 contain
  [[2 1][1 2]].  Thus in this case V includes one Latin square of order 3 
  and one of order 2 which is included three times.

  Our aim is to implement a function which accepts a vector of vectors V as
  an argument, and returns a map which keys and values are integers.  Each key
  should be the order of a Latin square included in V, and its value a count
  of different Latin squares of that order included in V.  If V does not
  include any Latin squares an empty map should be returned.  In the previous
  example the correct output of such a function is {3 1, 2 1} and not
  {3 1, 2 3}.
  ")

(def __
  (fn [input]
    (let [width (->> input
                     (map count)
                     (apply max))
          height (count input)

          ->lvals (->> (flatten input)
                       (into #{})
                       (map (fn [i k] [k (bit-set 0 i)]) (range))
                       (into {}))

          l-input (vec (map #(vec (map ->lvals %)) input))

          a-input (let [a (make-array Long/TYPE 64)]
                    (doseq [i (range 8)
                            j (range 8)
                            :let [index (+ j (* 8 i))
                                  value (get-in l-input [i j] 0)]]
                      (aset-long a index value))
                    a)

          square-mask (fn [w]
                        (->> (for [i (range w)
                                   j (range w)]
                               (bit-set 0 (+ j (* i 8))))
                             (reduce bit-or)))

          bits-in-mask (fn bits-in-mask [m]
                         (if (zero? m)
                           ()
                           (let [lsb (Long/lowestOneBit m)
                                 lsb-number (Long/numberOfTrailingZeros lsb)]
                             (conj (bits-in-mask (bit-xor m lsb)) lsb-number))))

          stripes [-72057594037927936
                   2r0000000011111111000000000000000000000000000000000000000000000000
                   2r0000000000000000111111110000000000000000000000000000000000000000
                   2r0000000000000000000000001111111100000000000000000000000000000000
                   2r0000000000000000000000000000000011111111000000000000000000000000
                   2r0000000000000000000000000000000000000000111111110000000000000000
                   2r0000000000000000000000000000000000000000000000001111111100000000
                   2r0000000000000000000000000000000000000000000000000000000011111111
                   -9187201950435737472
                   2r0100000001000000010000000100000001000000010000000100000001000000
                   2r0010000000100000001000000010000000100000001000000010000000100000
                   2r0001000000010000000100000001000000010000000100000001000000010000
                   2r0000100000001000000010000000100000001000000010000000100000001000
                   2r0000010000000100000001000000010000000100000001000000010000000100
                   2r0000001000000010000000100000001000000010000000100000001000000010
                   2r0000000100000001000000010000000100000001000000010000000100000001]

          square-masks (for [mask-width (range 2 9)
                             :when (<= mask-width width)
                             :when (<= mask-width height)

                             :let [sm (square-mask mask-width)]

                             i (range 0 8)
                             :when (<= (+ i mask-width) height)

                             j (range 0 8)
                             :when (<= (+ j mask-width) width)]
                         (let [mask (bit-shift-left sm (+ j (* i 8)))]
                           [mask-width
                            (cons mask
                                  (for [s stripes
                                        :let [m (bit-and s mask)]
                                        :when (not (zero? m))]
                                    m))]))

          alignments (loop [alignments [[]]
                            counts (map count input)]
                       (if-not (seq counts)
                         alignments
                         (recur
                           (for [n (range (inc (- width (first counts))))
                                 a alignments]
                             (vec (concat a [n])))
                           (rest counts))))

          alignment-mask (fn [alignment]
                           (->> (mapcat
                                  (fn [i j-offset]
                                    (let [j-length (count (get input i))]
                                      (for [j (range j-offset (+ j-offset j-length))]
                                        (bit-set 0 (+ j (* i 8))))))
                                  (range)
                                  alignment)
                                (reduce bit-or)))

          alignment-masks (map alignment-mask alignments)

          vset-for-mask (fn [mask alignment]
                          (loop [m mask
                                 v 0]
                            (if (zero? m)
                              v
                              (let [lsb (Long/lowestOneBit m)
                                    m-without-lsb (bit-xor m lsb)
                                    lsb-number (Long/numberOfTrailingZeros lsb)
                                    i (quot lsb-number 8)
                                    index (- lsb-number (get alignment i))

                                    ]
                                (recur
                                  m-without-lsb
                                  (bit-or v (aget ^longs a-input index)))))))

          values-for-mask (fn [mask alignment]
                            (->> mask
                                 bits-in-mask
                                 (map #(vector (quot % 8) (rem % 8)))
                                 (map (fn [[i j]]
                                        (get-in input [i (- j (get alignment i))])))))

          a-and-a-masks (map vector alignments alignment-masks)

          squares (for [[w [sm & _ :as all-masks]] square-masks
                        [a a-mask] a-and-a-masks
                        :when (= sm (bit-and a-mask sm))
                        :when (every? 
                                #(= w (Long/bitCount (vset-for-mask % a)))
                                all-masks)]
                    [w (values-for-mask sm a)])]
      (->> squares
           (into #{})
           (map first)
           frequencies))))

(comment
  (= (__ '[[A B C D]
           [A C D B]
           [B A D C]
           [D C A B]])
     {})
  (= (__ '[[A B C D E F]
           [B C D E F A]
           [C D E F A B]
           [D E F A B C]
           [E F A B C D]
           [F A B C D E]])
   {6 1})
  (= (__ '[[A B C D]
           [B A D C]
           [D C B A]
           [C D A B]])
   {4 1, 2 4})
  (= (__ '[[B D A C B]
           [D A B C A]
           [A B C A B]
           [B C A B C]
           [A D B C A]])
   {3 3})
  (= (__ [[2 4 6 3]
          [3 4 6 2]
          [6 2 4]])
   {})
  (= (__ [[1]
        [1 2 1 2]
        [2 1 2 1]
        [1 2 1 2]
        []       ])
   {2 2})
  (= (__ [[3 1 2]
          [1 2 3 1 3 4]
          [2 3 1 3]    ])
   {3 1, 2 2})
  (= (__ [[8 6 7 3 2 5 1 4]
        [6 8 3 7]
        [7 3 8 6]
        [3 7 6 8 1 4 5 2]
              [1 8 5 2 4]
              [8 1 2 4 5]])
   {4 1, 3 1, 2 7}))
