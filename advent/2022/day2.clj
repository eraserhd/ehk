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

(defn outcome-score [me them]
  (cond
   (= me them)       3   ; draw
   (beats [me them]) 6   ; win
   :else             0)) ; lose

(defn round-score [[them me]]
  (+ (shape-score me)
     (outcome-score me them)))

(defn compute [input]
  (->> input
       (map (partial mapv shapes))
       (map round-score)
       (reduce +)))

(comment
  (= 15 ())

  result)
  
 
