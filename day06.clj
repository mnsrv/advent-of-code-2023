(ns advent-of-code-2023.day06
  (:require
    [clojure.string :as str]
    [clojure.math :as math]))

(def sample1
  "Time:      7  15   30
Distance:  9  40  200")

(def input (slurp "inputs/day06"))

(defn parse [data]
  (->> data
    (str/split-lines)))

(defn equation [[t s]]
  (let [D  (- (math/pow t 2) (* 4 s))
        x1 (/ (+ t (math/sqrt D)) 2)
        x2 (/ (- t (math/sqrt D)) 2)]
    (if (and (= (math/floor x1) x1) (= (math/ceil x2) x2))
      (- (math/floor x1) (math/ceil x2) 1)
      (- (math/floor x1) (math/ceil x2) -1))
    ))

(defn part1 [data]
  (->> (parse data)
    (reduce (fn [result a]
              (conj result (map read-string (re-seq #"\d+" a))))
      '[])
    (apply map vector)
    (map equation)
    (reduce *)
    (int)
    ))

(part1 sample1)
(part1 input)


(defn debug [x]
  (println x)
  x)

(defn part2 [data]
  (->> (parse data)
    (reduce (fn [result a]
              (conj result (biginteger (str/join (re-seq #"\d+" a)))))
      '[])
    (equation)
    (biginteger)
    ))

(part2 sample1)
(part2 input)
