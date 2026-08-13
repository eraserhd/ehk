(ns value-at-risk
 (:require
  [scicloj.kindly.v4.kind :as kind]
  [scicloj.kindly.v4.api :as kindly])
 (:import
  (org.apache.commons.math3.distribution NormalDistribution)))

;; Value at Risk, or VaR or V@R, is a measure of the _size_ of investments.
;; Since it assumes normal distribution (or at least single-modality), it
;; doesn't model events that have causes other than normal market working.

^:kindly/hide-code
(def md (comp kindly/hide-code kind/md))
^:kindly/hide-code
(def tex (comp kindly/hide-code kind/tex))

(defn mean [series]
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
    (/ (* periods (mean rates))
       (* (Math/sqrt periods) (stddev rates)))))

(sharpe-ratio [100 107 99 102] 0.0001 1)

(md "## Continuously Compounded Returns")
(tex "x_t = \\ln\\left(\\frac{S_t}{S_{t-1}}\\right)")

(defn continuously-compounded-returns [prices]
  (for [[S_t-1 S_t] (partition 2 1 prices)]
    (Math/log (/ S_t S_t-1))))

(continuously-compounded-returns [100 107 99 102])

(md "## Volatility Calculation with Equal Probability")

(defn volatility [prices periods-per-year]
  (* (stddev (continuously-compounded-returns prices))
     (Math/sqrt periods-per-year)))

(volatility [100 107 99 102] 252)

(md "## Exponentially Weighted Volatility")

(tex "p_t = \\lambda^t")
(tex "\\mu = \\frac{\\sum_t{p_t x_t}}{\\sum_t{p_t}}")
(tex "\\sigma = \\sqrt{\\frac{\\sum_{t}{p_t(x_t - \\mu)^2}}{\\sum_{t}{p_t}}}")

(defn exponentially-weighted-volatility
  [λ prices]
  (let [x (continuously-compounded-returns prices)
        p (->> (map #(Math/pow λ %) (range))
               (take (count x))
               reverse)
        μ (/ (reduce + (map * p x))
             (reduce + p))]
    (Math/sqrt (/ (reduce + (map (fn [p_t x_t]
                                   (* p_t (Math/pow (- x_t μ) 2)))
                                 p
                                 x))
                  (reduce + p)))))

(md "## Parametric")

(def ^:private standard-normal (NormalDistribution. 0.0 1.0))

(defn parametric-var
  "Assumes percent returns are normally distributed, and independent from one period
  to the next (no mean reversion), and every period has the same volatility."
  [volatility size confidence-level]
  (- (* (.inverseCumulativeProbability standard-normal (- 1.0 confidence-level))
        size
        volatility)))

(one-day-parametric-var 0.05 100000.0 0.95)

(comment
  (* (/ 5.0 (.inverseCumulativeProbability (NormalDistribution. 0.0 1.0) 0.99))
     (.inverseCumulativeProbability (NormalDistribution. 0.0 1.0) 0.95)))
