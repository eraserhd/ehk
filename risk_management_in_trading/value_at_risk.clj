(ns value-at-risk)

;; Hello, this is a test.
;; Testing.

(defn percent-returns [prices]
  (for [[P_t-1 P_t] (partition 2 1 prices)]
    (- (/ P_t P_t-1) 1.0)))

(defn continuously-compounded-returns [prices]
  (for [[P_t-1 P_t] (partition 2 1 prices)]
    (Math/log (/ P_t P_t-1))))

(percent-returns [100 107 99 102])
(continuous-percent-returns [100 107 99 102])

(defn stddev [series]
  (-> (/ (->> series
              (map #(Math/pow % 2))
              (reduce + 0))
         (count series))
      (Math/pow 1/2)))

(defn volatility [prices periods-per-year]
  (stddev (continuously-compounded-returns prices)))

(volatility [100 107 99 102] 252)

