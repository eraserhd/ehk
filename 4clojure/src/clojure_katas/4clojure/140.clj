(ns clojure-katas.4clojure.140
  "Veitch, Please!

  Create a function which accepts as input a boolean algebra function in the
  form of a set of sets, where the inner sets are collections of symbols
  corresponding to the input boolean variables which satisfy the function (the
  inputs of the inner sets are conjoint, and the sets themselves are
  disjoint... also known as canonical minterms).  Note: capitalized
  symbols represent truth, and lower-case symbols represent negation of the
  inputs.  Your function must return the minimal function which is logically
  equivalent to the input.

  PS – You may want to give this a read before proceeding: <a
  href=\"http://en.wikipedia.org/wiki/K_map\">K-Maps</a>

  PPS – If
  you're interested in logic programming more generally, you should also check
  out: <a href=\"https://github.com/clojure/core.logic\">core.logic</a>")

;; A simple brute force would be simpler.  This is essentially brute force,
;; except that we try to do some K-map stuff first, which isn't strictly
;; necessary.

(def __
  (fn [input-fn]
    (let [bit (fn [n]
                (bit-shift-left 1 n))
          m-notation (fn [set-notation]
                       (bit-or
                         (if (set-notation 'A) (bit 0) 0)
                         (if (set-notation 'B) (bit 1) 0)
                         (if (set-notation 'C) (bit 2) 0)
                         (if (set-notation 'D) (bit 3) 0)))
          grey-map [2r0000 2r0001 2r0011 2r0010
                    2r0100 2r0101 2r0111 2r0110
                    2r1100 2r1101 2r1111 2r1110
                    2r1000 2r1001 2r1011 2r1010]
          bits (->> input-fn
                    (map m-notation)
                    (map grey-map)
                    (map bit)
                    (apply bit-or))
          shapes [2r1111111111111111 ; size 16
                  2r1111111100000000 ; size 8
                  2r1100110011001100
                  2r1111000000000000 ; size 4
                  2r1000100010001000
                  2r1100110000000000
                  2r1100000000000000 ; size 2
                  2r1000100000000000
                  2r1000000000000000]; size 1
          translate-right (fn [shape]
                            (bit-or
                              (bit-and
                                (bit-shift-right shape 1)
                                2r0111011101110111)
                              (bit-and
                                (bit-shift-left shape 3)
                                2r1000100010001000)))
          translate-down (fn [shape]
                           (bit-or
                             (bit-shift-right shape 4)
                             (bit-and
                               (bit-shift-left shape 12)
                               2r1111000000000000)))
          translate (fn [shape [i j]]
                      (nth (iterate translate-right (nth (iterate translate-down shape) i)) j))
          translate-positions (for [i (range 4)
                                    j (range 4)]
                                [i j])
          all-translations (fn [shape] (map #(translate shape %) translate-positions))
          patterns (distinct (mapcat all-translations shapes))
          applicable-patterns (filter #(= % (bit-and % bits)) patterns)
          consumed-patterns (into #{} (for [a applicable-patterns
                                            b applicable-patterns
                                            :when (and (not= a b) (= (bit-and a b) b))]
                                        b))

          largest-applicable-patterns (->> applicable-patterns
                                           (filter (complement consumed-patterns))
                                           (map-indexed #(vector (bit-set 0 %1) %2))
                                           (into {}))
          patterns-for (fn [n]
                         (->> n
                              (iterate #(bit-xor % (bit-and % (- %))))
                              (take-while (complement zero?))
                              (map #(bit-and % (- %)))
                              (map largest-applicable-patterns)))

          check-term (fn [pattern bits sym complement-sym]
                       (let [a (bit-and pattern bits)
                             b (bit-and-not pattern bits)]
                         (cond
                           (zero? a) [complement-sym]
                           (zero? b) [sym]
                           :else [])))

          symbols-used (reduce clojure.set/union input-fn)
          vars-used (set (concat
                           (if (or (symbols-used 'A) (symbols-used 'a)) '[A a])
                           (if (or (symbols-used 'B) (symbols-used 'b)) '[B b])
                           (if (or (symbols-used 'C) (symbols-used 'c)) '[C c])
                           (if (or (symbols-used 'D) (symbols-used 'd)) '[D d])))

          pattern->minterm (fn [pattern]
                             (clojure.set/intersection
                               vars-used
                               (set (concat
                                      (check-term pattern 2r0110011001100110 'A 'a)
                                      (check-term pattern 2r1100110011001100 'B 'b)
                                      (check-term pattern 2r0000111111110000 'C 'c)
                                      (check-term pattern 2r1111111100000000 'D 'd)))))

          answer (->> (range 1 (bit-set 0 (count largest-applicable-patterns)))
                      (map #(vector % (reduce bit-or (patterns-for %))))
                      (filter #(= bits (bit-and (second %) bits)))
                      first
                      first
                      patterns-for
                      (map pattern->minterm)
                      (into #{}))]
      answer)))

(= (__ #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'c}
     #{'A 'b}
     #{'B 'C 'd}})
(= (__ #{#{'A 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'B 'C}})
(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}})
   #{#{'a 'c}
     #{'A 'C}})
(= (__ #{#{'a 'b 'c}
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}})
   #{#{'a}})
(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'b 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'a 'B 'c 'd}
     #{'A 'B 'c 'D}
     #{'A 'b 'C 'D}
     #{'a 'b 'c 'D}
     #{'a 'B 'C 'D}
     #{'A 'B 'C 'd}})
(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}})
   #{#{'a 'c}
     #{'B 'c}})
(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'b 'C 'D}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'B 'C 'd}
         #{'A 'B 'C 'd}})
   #{#{'B 'd}
     #{'b 'D}})
(= (__ #{#{'a 'b 'c 'd}
         #{'A 'b 'c 'd}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'D}
         #{'a 'b 'C 'd}
         #{'A 'b 'C 'd}})
   #{#{'B 'D}
     #{'b 'd}})
