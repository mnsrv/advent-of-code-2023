(ns advent-of-code-2023.day07
  (:require
    [clojure.string :as str]
    [clojure.math :as math]))

(def sample1
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(def input (slurp "inputs/day07"))

(defn parse [data]
  (->> data
    (str/split-lines)))

(defn debug [x]
  (println x)
  x)

(defn hand-type [hand]
  (let [counts (vals (frequencies hand))]
    (cond 
      (some #{5} counts)                 7 ; Five of a kind
      (some #{4} counts)                 6 ; Four of a kind
      (every? #{3 2} counts)             5 ; Full house
      (some #{3} counts)                 4 ; Three of a kind
      (= 2 (get (frequencies counts) 2)) 3 ; Two pair
      (some #{2} counts)                 2 ; One pair
      :else                              1 ; High card
      )))

; A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2
(defn by-type-hand [a b]
  (let [points {"A" "m" "K" "l" "Q" "k" "J" "j" "T" "i" "9" "h" "8" "g" "7" "f" "6" "e" "5" "d" "4" "c" "3" "b" "2" "a"}]
    (compare [(:type a) (str/replace (:hand a) #"\w" points)]
      [(:type b) (str/replace (:hand b) #"\w" points)])))

(defn part1 [data]
  (->> (parse data)
    (map (fn [s]
           (let [[hand bid] (str/split s #" ")]
             (hash-map :hand hand :type (hand-type hand) :bid (Integer/parseInt bid)))))
    (sort by-type-hand)
    (map-indexed #(assoc %2 :rank (+ 1 %1)))
    (map #(* (:rank %) (:bid %)))
    (reduce + 0)))

(part1 sample1)
(part1 input)

(defn update-jokers [hand]
  (let [freqs-no-j (frequencies (str/replace hand #"J" ""))
        key        (if (empty? freqs-no-j)
                     \J
                     (key (apply max-key val freqs-no-j)))]
    (str/replace hand #"J" (str key))))

(defn hand-type-joker [hand]
  (let [new-hand (update-jokers hand)]
    (hand-type new-hand)))

; A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J
(defn by-type-hand-joker [a b]
  (let [points {"A" "m" "K" "l" "Q" "k" "J" "_" "T" "i" "9" "h" "8" "g" "7" "f" "6" "e" "5" "d" "4" "c" "3" "b" "2" "a"}]
    (compare [(:type a) (str/replace (:hand a) #"\w" points)]
      [(:type b) (str/replace (:hand b) #"\w" points)])))

(defn part2 [data]
  (->> (parse data)
    (map (fn [s]
           (let [[hand bid] (str/split s #" ")]
             (hash-map :hand hand :type (hand-type-joker hand) :bid (Integer/parseInt bid)))))
    (sort by-type-hand-joker)
    (map-indexed #(assoc %2 :rank (+ 1 %1)))
    (map #(* (:rank %) (:bid %)))
    (reduce + 0)))

(part2 sample1)
(part2 input)
