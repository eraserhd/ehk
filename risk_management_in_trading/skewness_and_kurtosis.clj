(ns skewness-and-kurtosis
 (:require
  [check :refer [check close-to]]
  [scicloj.kindly.v4.kind :as kind]
  [scicloj.kindly.v4.api :as kindly])
 (:import
  (org.apache.commons.math3.distribution NormalDistribution)))

^:kindly/hide-code
(def md (comp kindly/hide-code kind/md))
^:kindly/hide-code
(def tex (comp kindly/hide-code kind/tex))

(def test-series
  (let [dist (NormalDistribution. 0.0 1.0)]
    (into []
          (map (fn [_]
                 (.sample dist)))
          (range 100000))))

(md "## Mean & Standard Deviation")

(defn mean [series]
  (/ (reduce + series) (count series)))

(md "Standard deviation for samples:")

(tex "\\sigma = \\sqrt{\\frac{1}{N-1}\\sum_{i=1}^{N}{(x_i - \\bar{x}})^2}")

(defn stddev [samples]
  (let [center   (mean samples)
        centered (map #(- % center) samples)]
    (-> (transduce (map #(Math/pow % 2)) + centered)
        (/ (dec (count centered)))
        Math/sqrt)))

(check (mean test-series)
       (close-to 0 1e-2))

(check (stddev test-series)
       (close-to 1.0 1e-2))

(md "## Standardized Central Moments

Per [Wikipedia](https://en.wikipedia.org/wiki/Standardized_moment), a standardized
central moment of order k is:")

(tex "\\alpha_k = \\frac{\\mu_k}{\\sigma^k} = \\frac{E[(X - \\bar{x})^k]}{E[(X - \\bar{x})^2]^{k/2}}")

(defn standardized-central-moment
  [order series]
  (let [center   (mean series)
        centered (map #(- % center) series)]
    (/ (transduce (map #(Math/pow % order)) + centered)
       (dec (count centered))
       (Math/pow (stddev series) (/ order)))))

(md "### First Standardized Central Moment")

;; The first standardized central moment is always zero.
(check (standardized-central-moment 1 test-series)
       (close-to 0 1e-6))

(md "### Standard Deviation")

;; Standard deviation is the second standardized central moment.
(check (standardized-central-moment 2 test-series)
       (close-to (stddev test-series) 1e-2))

(md "### Skewness")

;; Skewness is the third standardized central moment.
(def skewness (partial standardized-central-moment 3))

;; The standard normal distribution has no skew.
(check (skewness test-series)
       (close-to 0 0.1))

(md "### Kurtosis")

;; Kurtosis is the fourth standardized central moment.
(def kurtosis (partial standardized-central-moment 4))

;; The kurtosis of the standard normal distribution is 3.
(check (kurtosis test-series)
       (close-to 3.0 0.1))
