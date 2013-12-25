(ns clojure-katas.4clojure.112
  "Sequs Horribilis

  Create a function which takes an integer and a nested collection of integers
  as arguments.  Analyze the elements of the input collection and return a
  sequence which maintains the nested structure, and which includes all
  elements starting from the head whose sum is less than or equal to the input
  integer.")

(def __

  (fn [limit sequence]
    (loop [state {:limit limit
                  :stack (list [sequence []])}]
      (letfn [(top-seq []
                (first (first (:stack state))))
              (top-result []
                (second (first (:stack state))))
              (finished? []
                (and (top-finished?)
                     (= 1 (count (:stack state)))))
              (top-finished? []
                (or
                  (< (limit) 0)
                  (empty? (top-seq))))
              (limit []
                (:limit state))
              (stack []
                (:stack state))
              (pop-top []
                (let [rest-of-stack (rest (rest (stack)))
                      [next-seq next-result] (second (stack))]
                  {:limit (limit)
                   :stack (conj
                            rest-of-stack
                            [next-seq (conj next-result (top-result))])}))
              (push-seq []
                {:limit (limit)
                 :stack (conj
                          (conj
                            (rest (stack))
                            [(rest (top-seq)) (top-result)])
                          [(first (top-seq)) []])})
              (do-number []
                (let [n (first (top-seq))
                      want? (<= n (limit))
                      new-limit (- (limit) n)
                      new-top-result (if want?
                                       (conj (top-result) n)
                                       (top-result))
                      new-stack (conj
                                  (rest (stack))
                                  [(rest (top-seq)) new-top-result])]
                  {:limit new-limit
                   :stack new-stack}))]
        (cond
          (finished?)
          (top-result)
          
          (top-finished?)
          (recur (pop-top))

          (coll? (first (top-seq)))
          (recur (push-seq))
          
          :else
          (recur (do-number))))))

  )

(=  (__ 10 [1 2 [3 [4 5] 6] 7])
   '(1 2 (3 (4))))
(=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
 '(1 2 (3 (4 (5 (6 (7)))))))
(=  (__ 9 (range))
   '(0 1 2 3))
(=  (__ 1 [[[[[1]]]]])
   '(((((1))))))
(=  (__ 0 [1 2 [3 [4 5] 6] 7])
   '())
(=  (__ 0 [0 0 [0 [0]]])
   '(0 0 (0 (0))))
(=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
   '(-10 (1 (2 3 (4)))))
