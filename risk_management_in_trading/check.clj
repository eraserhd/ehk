(ns check
 (:require
  [scicloj.kindly.v4.kind :as kind]))

(defn check [actual passes?]
  (kind/hiccup 
   [:div
    [:span {:style {:color "grey"}} ";=> " (pr-str actual)]
    [:span {:style {:font-weight "bold"}}
      " ["
      (if (passes? actual)
        [:span {:style {:color "green"}} "PASS"]
        [:span {:style {:color "red"}} "FAIL"])
      "]"]]
   {:code-and-value :horizontal}))

(defn close-to [goal delta]
  (fn [actual]
    (< (Math/abs (- goal actual)) delta)))
