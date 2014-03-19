(ns clojure-katas.4clojure.178
  "Best Hand

  Following on from <a href=\"http://www.4clojure.com/problem/128\">Recognize
  Playing Cards</a>, determine the best poker hand that can be made with five
  cards. The hand rankings are listed below for your convenience.

  * Straight flush: All cards in the same suit, and in sequence
  * Four of a kind: Four of the cards have the same rank
  * Full House: Three cards of one rank, the other two of another rank
  * Flush: All cards in the same suit
  * Straight: All cards in sequence (aces can be high or low, but not both
    at once)
  * Three of a kind: Three of the cards have the same rank
  * Two pair: Two pairs of cards have the same rank
  * Pair: Two cards have the same rank
  * High card: None of the above conditions are met
  ")

(def __
  (fn [hand]
    (let [kinds (->> hand
                     (map (fn [c] {(last c) #{(first c)}}))
                     (apply merge-with clojure.set/union)
                     vals
                     (map count)
                     sort
                     reverse)
          straight? (let [sorted-ranks-ace-low (->> hand
                                                    (map second)
                                                    (map {\A 1, \2 2, \3 3, \4 4, \5 5,
                                                          \6 6, \7 7, \8 8, \9 9, \T 10,
                                                          \J 11, \Q 12, \K 13})
                                                    sort)
                          sorted-ranks-ace-high (->> hand
                                                     (map second)
                                                     (map {\2 2, \3 3, \4 4, \5 5,
                                                           \6 6, \7 7, \8 8, \9 9, \T 10,
                                                           \J 11, \Q 12, \K 13, \A 14})
                                                     sort)
                          ranks-from-0-ace-low (map #(- % (first sorted-ranks-ace-low)) sorted-ranks-ace-low)
                          ranks-from-0-ace-high (map #(- % (first sorted-ranks-ace-high)) sorted-ranks-ace-high)]
                      (or (= [0 1 2 3 4] ranks-from-0-ace-low)
                          (= [0 1 2 3 4] ranks-from-0-ace-high)))
          flush? (->> hand
                      (map first)
                      sort
                      distinct
                      count
                      (= 1))]
      (cond
        (and straight? flush?)
        :straight-flush

        (= [4 1] kinds)
        :four-of-a-kind

        (= [3 2] kinds)
        :full-house

        flush?
        :flush

        straight?
        :straight

        (= [3 1 1] kinds)
        :three-of-a-kind

        (= [2 2 1] kinds)
        :two-pair

        (= [2 1 1 1] kinds)
        :pair

        :else
        :high-card))))

(comment
  (= :high-card (__ ["HA" "D2" "H3" "C9" "DJ"]))
  (= :pair (__ ["HA" "HQ" "SJ" "DA" "HT"]))
  (= :two-pair (__ ["HA" "DA" "HQ" "SQ" "HT"]))
  (= :three-of-a-kind (__ ["HA" "DA" "CA" "HJ" "HT"]))
  (= :straight (__ ["HA" "DK" "HQ" "HJ" "HT"]))
  (= :straight (__ ["HA" "H2" "S3" "D4" "C5"]))
  (= :flush (__ ["HA" "HK" "H2" "H4" "HT"]))
  (= :full-house (__ ["HA" "DA" "CA" "HJ" "DJ"]))
  (= :four-of-a-kind (__ ["HA" "DA" "CA" "SA" "DJ"]))
  (= :straight-flush (__ ["HA" "HK" "HQ" "HJ" "HT"])))
