(ns day8
 (:require
  [clojure.string :as s]))


(defn parse-board
  [text]
  (let [lines  (into [] (s/split-lines text))
        height (count lines)
        width  (count (first lines))
        coords (for [i (range 0 height)
                     j (range 0 width)]
                 [i j])
        cells  (reduce (fn [cells [i j]]
                         (assoc cells [i j] (- (long (get-in lines [i j])) (long \0))))
                       {}
                       coords)]
    {:height height,
     :width width,
     :cells cells}))

(def example-board
  (parse-board (str "30373\n"
                    "25512\n"
                    "65332\n"
                    "33549\n"
                    "35390\n")))

(def board (parse-board (slurp "day8.txt")))

(def all-directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn cells-in-direction
  "Does not include the starting cell."
  [{:keys [cells]} [i j] [di dj]]
  (->> (iterate (fn [[i j]] [(+ i di) (+ j dj)]) [(+ i di) (+ j dj)])
       (take-while (partial get cells))))

(defn visible-from-direction?
  [{:keys [cells] :as board} [i j] [di dj]]
  (let [this-tree-height    (get cells [i j])
        tallest-tree-height (->> (cells-in-direction board [i j] [di dj])
                                 (map (partial get cells))
                                 (reduce max -1))]
    (< tallest-tree-height this-tree-height)))

(defn visible?
  [{:keys [cells] :as board} [i j]]
  (->> all-directions
       (map #(visible-from-direction? board [i j] %))
       (reduce #(or %1 %2))))

(visible? example-board [4 4])

(defn solve [board]
  (->> (keys (:cells board))
       (filter (partial visible? board))
       count))

(= 21 (solve example-board))
(= 1763 (solve board))

(defn take-until [pred coll]
  (let [[head tail] (split-with (complement pred) coll)]
    (if (seq tail)
      (concat head (take 1 tail))
      head)))

(defn viewing-distance
  [{:keys [cells] :as board} [i j] [di dj]]
  (let [this-height (get cells [i j])]
    (->> (cells-in-direction board [i j] [di dj])
         (map (partial get cells))
         (take-until #(<= this-height %))
         count)))

(viewing-distance example-board [3 2] [-1 0])

(defn scenic-score
  [board [i j]]
  (->> all-directions
       (map #(viewing-distance board [i j] %))
       (reduce *)))

(defn solve2 [board]
  (->> (keys (:cells board))
       (map #(scenic-score board %))))

(solve2 example-board)
