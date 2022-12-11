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

(def start
  {:H [0 0]
   :T [0 0]
   :T-seen #{[0 0]}})

(def make-adjacent
  [])

(defn step [from to]
  (cond
   (= from to) from
   (< from to) (inc from)
   :else       (dec from)))

(defn move-once
  [{[hi hj] :H, [ti tj] :T, :as state} [di dj]]
  (let [hi (+ hi di)
        hj (+ hj dj)
        adjacent? (and (<= -1 (- hi ti) 1) (<= -1 (- hj tj) 1))
        [ti tj] (if adjacent?
                  [ti tj]
                  [(step ti hi) (step tj hj)])]
    (-> state
      (assoc :H [hi hj] :T [ti tj])
      (update :T-seen conj [ti tj]))))

(defn solve [moves]
  (let [end-state (reduce
                    move-once
                    start
                    (simplify-moves moves))]
    (count (:T-seen end-state))))

(= 13 (solve example-moves))
(= 6642 (solve moves))
