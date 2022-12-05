(ns day2)

(def input
  (read-string (slurp "day2.txt")))

(def shapes '{A :rock
              B :paper
              C :scissors
              X :rock
              Y :paper
              Z :scissors})

(def shape-score {:rock 1
                  :paper 2
                  :scissors 3})

(def beats #{[:rock :scissors]
             [:scissors :paper]
             [:paper :rock]})

(defn outcome [me them]
  (cond
   (= me them)       :draw
   (beats [me them]) :win
   :else             :lose))

(def outcome-score {:win 6, :draw 3, :lose 0})

(defn round-score [[them me]]
  (+ (shape-score me)
     (outcome-score (outcome me them))))

(defn compute [input]
  (->> input
       (map (partial mapv shapes))
       (map round-score)
       (reduce +)))

(comment
  (= 15 (compute '[[A Y] [B X] [C Z]])))


(def intended-outcome '{X :lose
                        Y :draw
                        Z :win})

(defn play [them intended-outcome]
  (->> [:rock :paper :scissors]
       (filter #(= intended-outcome (outcome % them)))
       first))

(defn compute2 [input]
  (->> input
       (map (fn [[them outcome]]
              [(shapes them) (intended-outcome outcome)]))
       (map (fn [[them outcome]]
              [them (play them outcome)]))
       (map round-score)
       (reduce +)))

(comment
  (play :rock :lose)
 
  (= 12 (compute2 '[[A Y] [B X] [C Z]]))

  (compute2 input))
