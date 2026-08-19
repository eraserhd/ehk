(ns black-scholes
 (:require
  [scicloj.kindly.v4.kind :as kind]
  [scicloj.kindly.v4.api :as kindly])
 (:import
  (org.apache.commons.math3.distribution NormalDistribution)))

^:kindly/hide-code
(def md (comp kindly/hide-code kind/md))
^:kindly/hide-code
(def tex (comp kindly/hide-code kind/tex))

(defn √ [^double x]
  (Math/sqrt x))
(defn Φ [^double x]
  (.cumulativeProbability (NormalDistribution. 0.0 1.0) x))

(md "## Black-Scholes Model

Black-Scholes is a model for predicting the prices of European options
(although it can be used for American options, since it's generally is
better to sell unexpired options than to exercise them).

")

(tex "C = Se^{(b-r)T}\\Phi(d_1) - Xe^{-rT}\\Phi(d_2)")
(tex "P = Xe^{-rT}\\Phi(-d_2) - Se^{(b-r)T}\\Phi(-d_1)")
(tex "d_1 = \\frac{\\ln{\\left(\\frac S X\\right)} + (b + \\frac 1 2\\sigma^2)T}{\\sigma\\sqrt T}")
(tex "d_2 = \\frac{\\ln{\\left(\\frac S X\\right)} + (b - \\frac 1 2\\sigma^2)T}{\\sigma\\sqrt T} = d_1 - \\sigma\\sqrt T")

(defn d_1 [S X b T σ]
  (/ (+ (Math/log (/ S X)) (* (+ b (* 1/2 σ σ)) T))
     (* σ (√ T))))

(defn d_2 [S X b T σ]
  (/ (+ (Math/log (/ S X)) (* (- b (* 1/2 σ σ)) T))
     (* σ (√ T))))

(defn C [S X b r T σ]
  (let [d_1 (d_1 S X b T σ)
        d_2 (d_2 S X b T σ)]
    (- (* S (Math/exp (* (- b r) T)) (Φ d_1))
       (* X (Math/exp (- (* r T))) (Φ d_2)))))

(defn P [S X b r T σ]
  (let [d_1 (d_1 S X b T σ)
        d_2 (d_2 S X b T σ)]
    (- (* X (Math/exp (- (* r T))) (Φ (- d_2)))
       (* S (Math/exp (* (- b r) T)) (Φ (- d_1))))))

(C 115 100 0 0.05 0.01 0.2) ;=> 14.992...
(P 100 105 0 0.05 0.01 0.2) ;=> 5.000...
