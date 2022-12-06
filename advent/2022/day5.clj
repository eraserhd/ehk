(ns day5
 (:require
  [clojure.string :as s]
  [clojure.walk :as w]))

(defn symbols->keywords [x]
  (w/postwalk (fn [x]
                (if (symbol? x)
                  (keyword (namespace x) (name x))
                  x))
              x))

(def input
  (let [[start-lines move-lines] (split-with (complement #{""}) (s/split-lines (slurp "day5.txt")))

        start-layers             (->> start-lines
                                      reverse
                                      (drop 1)
                                      (map (comp
                                            (partial into [])
                                            flatten
                                            (partial partition 1 4)
                                            (partial drop 1))))

        stacks                   (reduce
                                  (fn push-row* [stacks row]
                                    (reduce-kv
                                     (fn push-crate* [stacks index crate]
                                       (cond-> stacks
                                         (not= crate \space)
                                         (update (inc index) (partial cons crate))))
                                     stacks
                                     row))

                                  {}
                                  start-layers)

        moves                    (->> move-lines
                                      (drop 1)
                                      (map (comp symbols->keywords read-string #(str \{ % \}))))]
    {:state stacks
     :moves moves}))

(defn move-several [state {:keys [move from to]}]
  (let [crates (take move (get state from))]
    (-> state
        (update from (partial drop move))
        (update to (partial concat crates)))))

(defmulti move (fn [model state move] model))

(defmethod move :CrateMover9000
  [_ state {times :move, :as move}]
  (reduce (fn [state _]
            (move-several state (assoc move :move 1)))
          state
          (range times)))

(defmethod move :CrateMover9001
  [_ state move]
  (move-several state move))

(defn solve [model {:keys [state moves]}]
  (let [end-state (reduce (partial move model) state moves)]
    (->> end-state
         sort
         (map (comp first second))
         (apply str))))

(comment
 (= "VWLCWGSDQ" (solve :CrateMover9000 input))
 (= "TCGLQSLPW" (solve :CrateMover9001 input))
 input)
