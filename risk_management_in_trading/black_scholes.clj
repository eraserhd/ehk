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

(tex "V_\\text{call} = Se^{(b-r)T}\\Phi(d_1) - Xe^{-rT}\\Phi(d_2)")
(tex "V_\\text{put} = Xe^{-rT}\\Phi(-d_2) - Se^{(b-r)T}\\Phi(-d_1)")
(tex "d_1 = \\frac{\\ln{\\left(\\frac S X\\right)} + (b + \\frac 1 2\\sigma^2)T}{\\sigma\\sqrt T}")
(tex "d_2 = \\frac{\\ln{\\left(\\frac S X\\right)} + (b - \\frac 1 2\\sigma^2)T}{\\sigma\\sqrt T} = d_1 - \\sigma\\sqrt T")

(defn d_1 [S X b T σ]
  (/ (+ (Math/log (/ S X)) (* (+ b (* 1/2 σ σ)) T))
     (* σ (√ T))))

(defn d_2 [S X b T σ]
  (/ (+ (Math/log (/ S X)) (* (- b (* 1/2 σ σ)) T))
     (* σ (√ T))))

(defn V_call [S X b r T σ]
  (let [d_1 (d_1 S X b T σ)
        d_2 (d_2 S X b T σ)]
    (- (* S (Math/exp (* (- b r) T)) (Φ d_1))
       (* X (Math/exp (- (* r T))) (Φ d_2)))))

(defn V_put [S X b r T σ]
  (let [d_1 (d_1 S X b T σ)
        d_2 (d_2 S X b T σ)]
    (- (* X (Math/exp (- (* r T))) (Φ (- d_2)))
       (* S (Math/exp (* (- b r) T)) (Φ (- d_1))))))

(V_call 115 100 0 0.05 0.01 0.2) ;=> 14.992...
(V_put 100 105 0 0.05 0.01 0.2) ;=> 5.002...

(kind/plotly
 {:data [{:type :surface
          :z (for [S (range 95 105)]
               (for [T (map #(/ % 10.0) (range 100 0 -1))]
                 (V_call S 100 0 0.05 T 0.2)))}]
  :layout {"zaxis" {:title "Value"}}})

(md "## Greeks")
(md "### Delta")

(tex "\\Delta_\\text{call} = \\frac{\\partial V_\\text{call}}{\\partial S} = e^{(b-r)T}\\Phi(d_1)")
(tex "\\Delta_\\text{put} = \\frac{\\partial V_\\text{put}}{\\partial S} = e^{(b-r)T}\\left[\\Phi(d_1)-1\\right]")

(md "#### Derivation")
^:kindly/hide-code
(kind/hiccup
 [:div {:style {:border "solid black 1px"
                :width "100%"}}
  (tex "\\frac{\\partial{d_1}}{\\partial S}")
  (tex "= \\frac{\\partial}{\\partial S}\\frac{\\ln{\\left(\\frac S X\\right)} + (b + \\frac 1 2\\sigma^2)T}{\\sigma\\sqrt T}")
  (tex "= \\frac{\\sigma\\sqrt T\\frac{\\partial}{\\partial S}\\left[\\ln{\\left(\\frac S X\\right)} + (b + \\frac 1 2\\sigma^2)T\\right]- \\left[\\ln{\\left(\\frac S X\\right)} + (b + \\frac 1 2\\sigma^2)T\\right]\\frac{\\partial}{\\partial S}(\\sigma\\sqrt T)}
  {(\\sigma\\sqrt T)^2}")
  (tex "= \\frac{\\frac{\\partial}{\\partial S}\\left[\\ln{\\left(\\frac S X\\right)} + (b + \\frac 1 2\\sigma^2)T\\right]}{\\sigma\\sqrt T}")
  (tex "= \\frac{\\frac{\\partial}{\\partial S}\\ln{\\left(\\frac S X\\right)} + \\frac{\\partial}{\\partial S}(b + \\frac 1 2\\sigma^2)T}{\\sigma\\sqrt T}")
  (tex "= \\frac{\\frac X S\\frac{\\partial}{\\partial S}\\left(\\frac S X\\right)}{\\sigma\\sqrt T}")
  (tex "= \\frac{\\frac X S\\left(\\frac{1\\cdot X - 0\\cdot S}{X^2}\\right)}{\\sigma\\sqrt T}")
  (tex "= \\frac{\\frac 1 S}{\\sigma\\sqrt T}")
  (tex "= \\frac{1}{S\\sigma\\sqrt T}")

  ;; d2 = d1
  (tex "\\Delta_\\text{call} = \\frac{\\partial V_\\text{call}}{\\partial S}")
  (tex "= \\frac{\\partial}{\\partial S}\\left[Se^{(b-r)T}\\Phi(d_1) - Xe^{-rT}\\Phi(d_2)\\right]")
  (tex "= \\frac{\\partial}{\\partial S}\\left[Se^{(b-r)T}\\Phi(d_1)\\right] - \\frac{\\partial}{\\partial S}\\left[Xe^{-rT}\\Phi(d_2)\\right]")
  (tex "= \\frac{\\partial}{\\partial S}S\\left[e^{(b-r)T}\\Phi(d_1)\\right] + S\\frac{\\partial}{\\partial S}\\left[e^{(b-r)T}\\Phi(d_1)\\right] - \\left(\\frac{\\partial}{\\partial S}X\\left[e^{-rT}\\Phi(d_2)\\right] + X\\frac{\\partial}{\\partial S}\\left[e^{-rT}\\Phi(d_2)\\right]\\right)")
  (tex "= e^{(b-r)T}\\Phi(d_1) + S\\frac{\\partial}{\\partial S}\\left[e^{(b-r)T}\\Phi(d_1)\\right] - X\\frac{\\partial}{\\partial S}\\left[e^{-rT}\\Phi(d_2)\\right]")
  (tex "= e^{(b-r)T}\\Phi(d_1) + S\\left(\\frac{\\partial}{\\partial S}\\left[e^{(b-r)T}\\right]\\Phi(d_1) + e^{(b-r)T}\\frac{\\partial}{\\partial S}\\Phi(d_1)\\right)
   - X\\left(\\frac{\\partial}{\\partial S}\\left[e^{-rT}\\right]\\Phi(d_2) + e^{-rT}\\frac{\\partial}{\\partial S}\\Phi(d_2)\\right)")
  (tex "= e^{(b-r)T}\\Phi(d_1) + Se^{(b-r)T}\\frac{\\partial}{\\partial S}\\Phi(d_1) - Xe^{-rT}\\frac{\\partial}{\\partial S}\\Phi(d_2)")
  (tex "= e^{(b-r)T}\\Phi(d_1) + Se^{(b-r)T}\\varphi(d_1)\\frac{\\partial}{\\partial S}d_1 - Xe^{-rT}\\varphi(d_2)\\frac{\\partial}{\\partial S}d_2")
  (tex "= e^{(b-r)T}\\Phi(d_1) + \\frac{Se^{(b-r)T}\\varphi(d_1)}{S\\sigma\\sqrt T} - \\frac{Xe^{-rT}\\varphi(d_2)}{S\\sigma\\sqrt T}")])

(md "### Gamma")

(tex "\\Gamma = \\frac{\\partial^2 V}{{\\partial S}^2} = \\frac{e^{(b-r)T}\\varphi(d_1)}{S\\sigma\\sqrt T}")

(md "### Theta")

(tex "\\Theta_\\text{call} =
\\frac{\\partial{V_\\text{call}}}{\\partial T} =
- \\frac{Se^{(b-r)T}\\varphi(d_1)\\sigma}{2\\sqrt T}
- (b - r)Se^{(b-r)T}\\Phi(d_1)
- rXe^{-rT}\\Phi(d_2)")

(tex "\\Theta_\\text{put} =
\\frac{\\partial{V_\\text{put}}}{\\partial T} =
- \\frac{Se^{(b-r)T}\\varphi(d_1)\\sigma}{2\\sqrt T}
+ (b - r)Se^{(b-r)T}\\Phi(-d_1)
+ rXe^{-rT}\\Phi(-d_2)")

(md "### Vega")

(tex "\\Lambda =
\\frac{\\partial V}{\\partial\\sigma} =
Se^{(b-r)T}\\varphi(d_1)\\sqrt T")

(md "### Rho")

(tex "\\rho_\\text{call} = \\frac{\\partial V_\\text{call}}{\\partial r} = TXe^{-rT}\\Phi(d_2)")
(tex "\\rho_\\text{put} = \\frac{\\partial V_\\text{put}}{\\partial r} = -TXe^{-rT}\\Phi(-d_2)")

(md "### Phi / Rho* / Rho-2")

(tex "\\rho_{2\\text{call}} = \\frac{\\partial V_\\text{call}}{\\partial b} = -TSe^{(b-r)T}\\Phi(d_1)")
(tex "\\rho_{2\\text{put}} = \\frac{\\partial V_\\text{put}}{\\partial b} = TSe^{(b-r)T}\\Phi(-d_1)")
