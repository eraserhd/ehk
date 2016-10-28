(ns life-clj.core
  (:require [clojure.set :as set]))

(defn board->set
  [board]
  (set (for [i (range (count board))
             :let [row (get board i)]
             j (range (count row))
             :when (not= \space (get row j))]
         [i j])))

(defn- normalize
  "Chooses a constant amount by which to move all alive cells so that
  we have no negative coordinates."
  [alive]
  (let [di (- (transduce (map first) min Long/MAX_VALUE alive))
        dj (- (transduce (map second) min Long/MAX_VALUE alive))]
    (into #{} (map (fn [[i j]] [(+ i di) (+ j dj)])) alive)))

(defn set->board
  [alive]
  (let [alive (normalize alive)
        height (inc (transduce (map first) max 0 alive))
        width (inc (transduce (map second) max 0 alive))
        empty-row (vec (repeat width \space))
        empty-board (vec (repeat height empty-row))]
    (->> alive
      (reduce (fn [board [i j]]
                (assoc-in board [i j] \X))
              empty-board)
      (mapv (partial apply str)))))

(def deltas
  "Relative coordinates for each neighbording cell, e.g. [+1 +1]
  or [-1 0] (but not [0 0])."
  (for [delta-i [-1 0 +1]
        delta-j [-1 0 +1]
        :when (not= 0 delta-i delta-j)]
     [delta-i delta-j]))

(defn- neighbors
  [alive]
  (for [[i j] alive
        [di dj] deltas]
    [(+ i di) (+ j dj)]))

(defn step
  [alive]
  (let [neighbors (reduce
                    #(update %1 %2 (fnil inc 0))
                    {}
                    (neighbors alive))
        lives? (fn [p] (<= 2 (or (get neighbors p) 0) 3)) 
        born (into #{}
                    (comp
                      (filter #(= 3 (second %)))
                      (map first)
                      (remove alive))
                    neighbors)]
    (set/union (filter lives? alive) born)))
