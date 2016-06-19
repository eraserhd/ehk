(ns clojure-katas.4clojure.115
  "The Balance of N

  A balanced number is one whose component digits have the same sum on the
  left and right halves of the number.  Write a function which accepts an
  integer n, and returns true iff n is balanced.")

(def __

  (fn [n]
    (let [ds (->> n
                  (iterate #(quot % 10))
                  (take-while #(> % 0))
                  (map #(mod % 10))
                  (into []))]
      (loop [left 0
             right 0
             ds ds]
        (if (>= 1 (count ds))
          (= left right)
          (recur (+ left (first ds))
                 (+ right (last ds))
                 (subvec ds 1 (dec (count ds)))))))))

(comment
  (= true (__ 11))
  (= true (__ 121))
  (= false (__ 123))
  (= true (__ 0))
  (= false (__ 88099))
  (= true (__ 89098))
  (= true (__ 89089))
  (= (take 20 (filter __ (range)))
     [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])  )
