(ns financial-mathematics
 (:require
  [scicloj.kindly.v4.kind :as kind]
  [scicloj.kindly.v4.api :as kindly])
 (:import
  (org.apache.commons.math3.distribution NormalDistribution)))

^:kindly/hide-code
(def md (comp kindly/hide-code kind/md))
^:kindly/hide-code
(def tex (comp kindly/hide-code kind/tex))

(defn mean [series]
  (/ (reduce + series) (count series)))

(md "## Standardized Central Moments

Per [Wikipedia](https://en.wikipedia.org/wiki/Standardized_moment):
    ")

(tex "\\alpha_k = \\frac{\\mu_k}{\\sigma^k} = \\frac{E[(X - \\mu)^k]}{E[(X - \\mu)^2]^{k/2}}")

(defn standardized-central-moment
  [order series]
  (let [center   (mean series)
        centered (map #(- % center) series)
        stddev   (-> (transduce (map #(Math/pow % 2)) + centered)
                     (/ (count centered))
                     Math/sqrt)]
    (/ (transduce (map #(Math/pow % order)) + centered)
       (Math/pow stddev (/ order)))))

(def stddev (partial standardized-central-moment 2))
(def skewness (partial standardized-central-moment 3))
(def kurtosis (partial standardized-central-moment 4))

(def test-series
  (let [dist (NormalDistribution. 0.0 1.0)]
    (into []
          (map (fn [_]
                 (.sample dist)))
          (range 100000))))

(defn check [actual passes?]
  (let [color (if (passes? actual)
                "green"
                "red")]
    (kind/hiccup
     [:div {:style {:background-color color}} actual])))

(defn close-to [goal delta]
  (fn [actual]
    (< (Math/abs (- goal actual)) delta)))

;; The first standardized central moment is always zero
(check (standardized-central-moment 1 test-series)
       (close-to 0 1e-6))

(check (mean test-series)
       (close-to 0 1e-2))

(check (stddev test-series)
       (close-to 1.0 1e-6))
(skewness test-series)

(check (kurtosis test-series)
       (close-to 3.0 0.1))

(tex "\\frac{\\delta\\left[\\frac{\\delta L}{\\delta t}\\right]}{\\delta t}")

(tex "\\frac{\\left[\\frac{\\delta^2 L}{\\delta t}\\right]}{\\delta t}")

(tex "\\frac{\\delta^2 L}{\\delta t}\\cdot\\frac{1}{\\delta t}")

(tex "\\frac{\\delta^2 L}{\\delta^2 t^2}")
