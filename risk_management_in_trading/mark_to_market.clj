(ns mark-to-market
 (:require
  [financial-mathematics :as m]))

(defn stddev [series]
  (-> (/ (->> series
              (map #(Math/pow % 2))
              (reduce + 0))
         (count series))
      (Math/pow 1/2)))

(defn percent-returns [prices]
  (for [[P_t-1 P_t] (partition 2 1 prices)]
    (- (/ P_t P_t-1) 1.0)))

(percent-returns [100 107 99 102])

(defn sharpe-ratio [prices risk-free-rate periods]
  (let [rates (map #(- % risk-free-rate) (percent-returns prices))]
    (/ (* periods (m/mean rates))
       (* (Math/sqrt periods) (stddev rates)))))
