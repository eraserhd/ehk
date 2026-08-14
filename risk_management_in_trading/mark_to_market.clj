(ns mark-to-market
 (:require
  [skewness-and-kurtosis :as m]))

(defn percent-returns [prices]
  (for [[P_t-1 P_t] (partition 2 1 prices)]
    (- (/ P_t P_t-1) 1.0)))

(percent-returns [100 107 99 102])

(defn sharpe-ratio [prices risk-free-rate periods]
  (let [rates (map #(- % risk-free-rate) (percent-returns prices))]
    (/ (* periods (m/mean rates))
       (* (Math/sqrt periods) (m/stddev rates)))))
