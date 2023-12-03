(ns advent-of-code-2023.day02
  (:require
    [clojure.string :as str]))

(def sample1
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def input (slurp "inputs/day02"))

(defn parse [data]
  (->> data
    (str/split-lines)))

(def maxim {:red   12
            :green 13
            :blue  14})

(defn count-cubes [set]
  (->> set
    (re-seq #"(\d+) (\w+)")
    (map (fn [[_ v k]] [(keyword k) (Integer/parseInt v)]))
    (into {})))

(defn is-possible? [dict]
  (and (<= (get dict :red) (get maxim :red))
    (<= (get dict :green) (get maxim :green))
    (<= (get dict :blue) (get maxim :blue))))

(defn get-min-cubes [s]
  (->> (str/split (second (str/split s #": ")) #"; ")
    (map count-cubes)
    (reduce #(merge-with max %1 %2))))

(defn check-game [s]
  (let [game (second (str/split (first (str/split s #": ")) #" "))
        min-cubes (get-min-cubes s)]
    (if (is-possible? min-cubes) (Integer/parseInt game) 0)))

(defn part1 [data]
  (->> (parse data)
    (map check-game)
    (reduce + 0)))

(part1 sample1)
(part1 input)

(defn part2 [data]
  (->> (parse data)
    (map get-min-cubes)
    (map vals)
    (map #(reduce * %))
    (reduce + 0)))

(part2 sample1)
(part2 input)


