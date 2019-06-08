(defn fork
  [& xforms]
  (fn [rf]
    (let [rfs (map #(% rf) xforms)]
      (fn fork*
        ([] (map #(%) rfs))
        ([result] (map rfs result))
        ([result input] (map rfs result (repeat input)))))))