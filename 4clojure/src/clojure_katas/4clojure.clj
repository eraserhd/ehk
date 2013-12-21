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

(defn tests
  [problem]
  (str
    (string/join "\n" (map string/trim-newline (:tests problem)))
    "\n"))

(defn word-wrap
  "Wraps a paragraph at 76 characters."
  [paragraph]
  (loop [string paragraph
         lines []]
    (cond
      (= "" string)
      (string/join "\n" lines)
      (>= 76 (count string))
      (recur "" (conj lines string))
      :else
      (let [break-point (or (first
                              (for [i (range 76 0 -1)
                                    :when (= \space (get string i))]
                                i))
                            76)
            [line rest] (split-at break-point string)
            line (string/trim (apply str line))
            rest (string/trim (apply str rest))]
        (recur rest (conj lines line))))))

(defn format-description
  [description]
  (-> description
      (string/replace #"</?code>" "")
      (string/replace #"<p>" "")
      (string/split #"</p>")
      ((partial map word-wrap))
      ((partial string/join "\n\n"))
      string/trim-newline))

(defn indent-string
  [s]
  (->> s
       (#(string/split % #"\n"))
       (map #(str "  " %))
       (string/join "\n")
       (#(string/replace % #"  \n" "\n"))))

(defn ns-docstring
  [problem]
  (str (:title problem) "\n\n"
       (-> (:description problem)
           format-description
           indent-string)))

(defn problem-ns
  [problem]
  (str "(ns clojure-katas.4clojure." (:number problem) "\n"
       "  \""
       (-> (ns-docstring problem)
           (string/replace "\\" "\\\\")
           (string/replace "\"" "\\\""))
       "\")\n"))

(defn problem-file-content
  [problem]
  (str (problem-ns problem)
       "\n"
       "(def __\n"
       "\n"
       "  (fn []\n"
       "    nil)\n"
       "\n"
       "  )\n"
       "\n"
       (tests problem)))

(defn problem-path
  [problem]
  (str "src/clojure_katas/4clojure/" (:number problem) ".clj"))

(defn start-problem
  "Create starter file for working on a 4clojure problem."
  [problem-number]
  (let [problem (get-problem problem-number)
        path (problem-path problem)]
    (spit path (problem-file-content problem))
    (println (str "Wrote " path "."))))
