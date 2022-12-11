(ns day9)

(defn parse-moves [text]
  (partition 2 (read-string (str "(" text ")"))))

(def example-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def example-moves (parse-moves example-input))
(def moves (parse-moves (slurp "day9.txt")))

(def directions
  '{U [-1  0]
    D [+1  0]
    L [ 0 -1]
    R [ 0 +1]})

(defn simplify-moves [moves]
  (mapcat (fn [[dir n]]
            (repeat n (get directions dir)))
          moves))

(defn start [length]
  (repeat length {:at [0 0], :seen #{[0 0]}}))

(defn step [from to]
  (cond
   (= from to) from
   (< from to) (inc from)
   :else       (dec from)))

(defn fix-tail
  [[hi hj] tail]
  (if-not (seq tail)
    ()
    (let [[{[ti tj] :at, :as next} & more-tail] tail
          adjacent? (and (<= -1 (- hi ti) 1) (<= -1 (- hj tj) 1))
          [ti tj]   (if adjacent?
                      [ti tj]
                      [(step ti hi) (step tj hj)])
          next'     (-> next
                        (assoc :at [ti tj])
                        (update :seen conj [ti tj]))]
      (cons next' (fix-tail [ti tj] more-tail)))))

(defn move-one-step
  [[{[hi hj] :at, :as head} & tail] [di dj]]
  (let [hi    (+ hi di)
        hj    (+ hj dj)
        head' (assoc head :at [hi hj])]
    (cons head' (fix-tail [hi hj] tail))))

(defn solve [moves length]
  (let [end-rope (reduce
                   move-one-step
                   (start length)
                   (simplify-moves moves))]
    (count (:seen (last end-rope)))))

(= 13 (solve example-moves 2))
(= 6642 (solve moves 2))

(def larger-example
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")
(def larger-moves (parse-moves larger-example))

(= 36 (solve larger-moves 10))
(= 2765 (solve moves 10))
