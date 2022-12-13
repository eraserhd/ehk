(ns day12
 (:require
  [clojure.string :as s]))

(def input (slurp "day12.txt"))
(def example-input
  (str "Sabqponm\n"
       "abcryxxl\n"
       "accszExk\n"
       "acctuvwj\n"
       "abdefghi\n"))

(def char->height
  (into {\S 0
         \E 25}
        (for [h (range 26)]
          [(char (+ h (int \a))) h])))

(def directions [[0 1] [0 -1] [1 0] [-1 0]])

(defn parse-input [input]
  (let [lines             (-> input
                              s/split-lines)
        height            (count lines)
        width             (count (first lines))
        chars-with-coords (into {} (for [i (range 0 height)
                                         j (range 0 width)
                                         :let [ch (get-in lines [i j])]]
                                     [[i j] ch]))
        heightmap         (reduce-kv #(assoc %1 %2 (get char->height %3))
                                     {}
                                     chars-with-coords)
        start             (->> chars-with-coords
                               (filter (comp #{\S} val))
                               ffirst)
        end               (->> chars-with-coords
                               (filter (comp #{\E} val))
                               ffirst)
        all-lowest        (->> heightmap
                               (filter (comp zero? val))
                               (map key))]
    {:heightmap    heightmap
     :marked-start [start]
     :all-lowest   all-lowest
     :end          end}))

(defn shortest-path [input how]
  (let [{:keys [end heightmap], :as parsed} (parse-input input)
        starts (parsed how)
        queue  (into (clojure.lang.PersistentQueue/EMPTY) starts)
        seen   (reduce #(assoc %1 %2 0) {} starts)]
    (loop [queue queue
           seen  seen]
      (cond
       (empty? queue)
       nil

       (= end (peek queue))
       (get seen (peek queue))

       :else
       (let [[i j]  (peek queue)
             queue  (pop queue)
             height (get heightmap [i j])
             steps' (inc (seen [i j]))
             next   (->> directions
                         (map (fn [[di dj]] [(+ i di) (+ j dj)]))
                         (filter (complement seen))
                         (filter heightmap)
                         (filter #(<= (get heightmap %) (inc height))))
             queue  (into queue next)
             seen   (into seen (map (fn [n] [n steps']) next))]
         (recur queue seen))))))

(= 31 (shortest-path example-input :marked-start))
(= 420 (shortest-path input :marked-start))

(= 29 (shortest-path example-input :all-lowest))
(= 414 (shortest-path input :all-lowest))
