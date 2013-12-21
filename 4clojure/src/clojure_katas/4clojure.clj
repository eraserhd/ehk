(ns clojure-katas.4clojure
  (:require [clojure.string :as string])
  (:require [clj-http.client :as http])
  (:require [cheshire.core :as json]))

(defn get-problem
  "Retrieves a problem using 4clojure's API.  For example,

    {:restricted [],
     :title \"Sequence of pronunciations\",
     :times-solved 34,
     :difficulty \"Medium\",
     :scores
     {:101 0, :87 0, :117 1, :158 0, :159 1, :285 1, :83 0, :397 0, ...},
     :tests
     [\"(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))\"
      ...],
     :user \"mlni\",
     :number 110,
     :description \"<p>Write a function that returns[...]\",
     :tags [\"seqs\"]}
  "
  [number]
  (let [url (str "http://www.4clojure.com/api/problem/" number)
        response (http/get url)]
    (when (= 200 (:status response))
      (-> (:body response)
          (json/parse-string true)
          (assoc :number number)))))

(defn problem-ns
  [problem]
  (str "(ns clojure-katas.4clojure." (:number problem) "\n"
       "  " (pr-str (:title problem)) ")\n"))

(defn tests
  [problem]
  (str
    (string/join "\n" (map string/trim-newline (:tests problem)))
    "\n"))

(defn format-description
  [description]
  (-> description
      (string/replace #"</?code>" "")
      (string/replace #"<p>" "")
      (string/replace #"</p>" "\n\n")
      string/trim-newline
      (str "\n")))
