
(let [l [
"(let [l ["
"         ]]"
"  (mapv println (subvec l 0 1))"
"  (mapv prn l)"
"  (mapv println (subvec l 1)))"
         ]]
  (mapv println (subvec l 0 1))
  (mapv prn l)
  (mapv println (subvec l 1)))

(let [q (quote (let 0
                 (map
                   (fn [x] (if (= 0 x) [(quote q) (list (quote quote) q)] x))
                   q)))]
  (map
    (fn [x] (if (= 0 x) [(quote q) (list (quote quote) q)] x))
    q))

((fn [x] (list x (list (quote quote) x)))
 (quote (fn [x] (list x (list (quote quote) x)))))
