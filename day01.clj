(ns advent-of-code-2023.day01
  (:require
    [clojure.string :as str]))

(def sample1
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def input (slurp "inputs/day01"))

(defn parse [data]
  (->> data
    (str/split-lines)))

(defn get-digits [s]
  (let [digits (re-seq #"\d" s)
        a      (first digits)
        b      (last digits)]
    (Integer/parseInt (str a b))))

(defn part1 [data]
  (->> (parse data)
    (map get-digits)
    (reduce + 0)))

(part1 sample1)
(part1 input)

(def sample2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def digits-dict
  {"1"     "1"
   "2"     "2"
   "3"     "3"
   "4"     "4"
   "5"     "5"
   "6"     "6"
   "7"     "7"
   "8"     "8"
   "9"     "9"
   "0"     "0"
   "one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})

(defn get-digits-2 [s]
  (let [p1 (re-pattern (str/join "|" (keys digits-dict)))
        p2 (re-pattern (str/join "|" (map str/reverse (keys digits-dict))))
        a  (digits-dict (re-find p1 s))
        b  (digits-dict (str/reverse (re-find p2 (str/reverse s))))]
    (Integer/parseInt (str a b))))

(defn part2 [data]
  (->> (parse data)
    (map get-digits-2)
    (reduce + 0)))

(part2 sample2)
(part2 input)




