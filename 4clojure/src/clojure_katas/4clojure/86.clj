(ns clojure-katas.4clojure.86)

(def happy?

  (fn [n]
    (letfn [(digits [n]
              (if (= n 0)
                ()
                (cons (mod n 10) (digits (quot n 10)))))]
      (loop [n n
             seen #{}]
        (cond
          (= n 1) true
          (seen n) false
          :else (recur
                  (reduce + (map #(* % %) (digits n)))
                  (conj seen n))))))

  )


(= (happy? 7) true)
(= (happy? 986543210) true)
(= (happy? 2) false)
(= (happy? 3) false)
