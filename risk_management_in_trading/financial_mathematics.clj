(ns financial-mathematics)

(defn mean [series]
  (/ (reduce + series) (count series)))
