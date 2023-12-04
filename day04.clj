(ns advent-of-code-2023.day04
  (:require
    [clojure.string :as str]
    [clojure.math :as math]
    [clojure.set :as set]))

(def sample1
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def input (slurp "inputs/day04"))

(defn parse [data]
  (->> data
    (str/split-lines)))

(defn count-matches [card]
  (let [[winning my-numbers] (str/split (second (str/split card #": ")) #" \| " )]
    (->> (set (re-seq #"\d+" my-numbers))
      (set/intersection (set (re-seq #"\d+" winning)))
      (count)
      )))
; (count-matches "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
; 4

(defn count-points [m]
  (if (> m 0)
    (int (math/pow 2 (- m 1)))
    0))
; (count-points 4)
; 8


(defn part1 [data]
  (->> (parse data)
    (map count-matches)
    (map count-points)
    (reduce + 0)
    ))

(part1 sample1)
(part1 input)


(defn debug [x]
  (println x)
  x)

(defn fill-with-0 [limit start original-list]
  (let [start-len (+ 1 start)
        start-0   (repeat start-len 0)
        end-len   (- limit start-len (count original-list))
        end-0     (repeat end-len 0)]
    (concat start-0 original-list end-0)))
; (fill-with-zeroes 6 0 '(1 1 1 1))
; (0 1 1 1 1 0)
; (fill-with-zeroes 6 1 '(2 2))
; (0 0 2 2 0 0)
; (fill-with-zeroes 6 5 '())
; (0 0 0 0 0 0)

(defn count-cards [indexed-list]
  (let [list-size (count indexed-list)]
    (reduce (fn [result [i v]]
              (->> (repeat v (nth result i))
                (fill-with-zeroes (count result) i)
                (map + result)))
      (repeat list-size 1)
      indexed-list)))
; (count-cards '([0 4] [1 2] [2 2] [3 1] [4 0] [5 0]))
; (1 2 4 8 14 1)


(defn part2 [data]
  (->> (parse data)
    (map count-matches)
    (map-indexed vector)
    (count-cards)
    (reduce + 0)
    ))

(part2 sample1)
(part2 input)

; (1 1 1 1 1 1)
; (0 1 1 1 1 0) 4   0 1 * 4
; (0 0 2 2 0 0) 2   1 2 * 2
; (0 0 0 4 4 0) 2   2 4 * 2
; (0 0 0 0 8 0) 1   3 8 * 1
; (0 0 0 0 0 0) 0   4 1 * 0
; (- - - - - -)
; (1 2 4 8 14 1) 30
