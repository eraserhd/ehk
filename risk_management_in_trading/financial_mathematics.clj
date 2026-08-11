(ns financial-mathematics
 (:import
  (org.apache.commons.math3.distribution NormalDistribution)))

(defn mean [series]
  (/ (reduce + series) (count series)))

(defn standardized-central-moment
  [order series]
  (let [center (mean series)]
    (/ (transduce (map #(Math/pow (- % center) order)) + series)
       (-> (transduce (map #(Math/pow (- % center) 2)) + series)
           (Math/pow (/ order 2))))))

(def stddev (partial standardized-central-moment 2))
(def skewness (partial standardized-central-moment 3))
(def kurtosis (partial standardized-central-moment 4))

(def test-series
  (let [dist (NormalDistribution. 0.0 1.0)]
    (into []
          (map (fn [_]
                 (.sample dist)))
          (range 100000))))

;; The first standardized central moment is always zero
(standardized-central-moment 1 test-series)

;; Should be close to zero
(mean test-series)
(stddev test-series)
(skewness test-series)
(kurtosis test-series)
