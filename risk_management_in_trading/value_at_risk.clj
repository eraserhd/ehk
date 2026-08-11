(ns value-at-risk
 (:import
  (org.apache.commons.math3.distribution NormalDistribution)))

;; Value at Risk, or VaR or V@R, is a measure of the _size_ of investments.
;; Since it assumes normal distribution (or at least single-modality), it
;; doesn't model events that have causes other than normal market working.

(defn average [series]
  (/ (reduce + series) (count series)))

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
    (/ (* periods (average rates))
       (* (Math/sqrt periods) (stddev rates)))))

(sharpe-ratio [100 107 99 102] 0.0001 1)

(defn continuously-compounded-returns [prices]
  (for [[P_t-1 P_t] (partition 2 1 prices)]
    (Math/log (/ P_t P_t-1))))

(continuously-compounded-returns [100 107 99 102])

(defn volatility [prices periods-per-year]
  (stddev (continuously-compounded-returns prices)))

(volatility [100 107 99 102] 252)

(defn one-day-parametric-var
  "Assumes percent returns are normally distributed, and independent from one period
  to the next (no mean reversion), and every period has the same volatility."
  [volatility size confidence-level]
  (* (.inverseCumulativeProbability (NormalDistribution. 0.0 1.0) (- 1.0 0.05))
     size
     volatility))

(one-day-parametric-var 0.05 100000.0 0.95)
