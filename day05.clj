(ns advent-of-code-2023.day05
  (:require
    [clojure.string :as str]))

(def sample1
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def input (slurp "inputs/day05"))

(defn parse [data]
  (->> data
    (str/split-lines)))

(defn get-value [value l]
  (reduce
    (fn [result [destination source range]]
      (if (<= source value (+ source range -1))
        (reduced (+ destination (- value source)))
        result))
    value
    l))
(get-value 53 '((57 7 4) (42 0 7) (0 11 42) (49 53 8)))
; 49

(defn get-interval-from-map [[destination source range]]
  (list source (+ source range -1)))

(defn get-interval-from-seed  [[source range]]
  (list source (+ source range -1)))

(defn interval-inside? [[a b] [c d]]
  (<= c a b d))
; [ (+) ]

(defn interval-outside? [[a b] [c d]]
  (or (> a d) (< b c)))
; (-) [ ]
; [ ] (-)

(defn split-interval [[a b] [c d]]
  (cond
    ; 1 3  5 7    1 23  56 7
    ; (-[++]-) -> (-)(++)(-)
    (and (< a c) (> b d)) (list (list a (- c 1)) (list c d) (list (+ d 1) b))
    ; [  (+]-) -> (+)(-)
    (<= c a d b) (list (list a d) (list (+ d 1) b))
    ; (-[+)  ] -> (-)(+)
    (<= a c b d) (list (list a (- c 1)) (list c b))
    :else (list)))

(split-interval '(57 69) '(53 60))
; 53 57 60 69
;  [  (  ]  )
(split-interval '(53 60) '(57 69))
; 53 57 60 69
;  (  [  )  ]
(split-interval '(53 69) '(57 60))
; 53 57 60 69
;  (  [  ]  )

(defn create-maps [data]
  (reduce (fn [result row]
            (cond
              (str/includes? row "seeds") result
              ; (str/includes? row "map") (assoc result :step (first (str/split row #" ")))
              (str/includes? row "map") (conj result ())
              ; (re-seq #"\d+" row) (assoc result (keyword (get result :step)) (conj (get result (keyword (get result :step))) (map read-string (str/split row #" "))))
              (re-seq #"\d+" row) (conj (pop result) (conj (peek result) (map read-string (str/split row #" "))))
              :else result))
    []
    data
    ))
(create-maps (parse sample1))
; [((52 50 48) (50 98 2)) ((39 0 15) (37 52 2) (0 15 37)) ((57 7 4) (42 0 7) (0 11 42) (49 53 8)) ((18 25 70) (88 18 7)) ((68 64 13) (81 45 19) (45 77 23)) ((1 0 69) (0 69 1)) ((56 93 4) (60 56 37))]


(defn part1 [data]
  (let [l     (parse data)
        seeds (map read-string (re-seq #"\d+" (first l)))]
    (->> l
      (create-maps)
      (reduce
        (fn [result l]
          (map #(get-value % l) result))
        seeds)
      (apply min)
      )))

(part1 sample1)
(part1 input)


(defn debug [x]
  (println x)
  x)

(defn convert [interval maps]
  (reduce (fn [result m]
            (cond
              (interval-outside? interval (get-interval-from-map m)) result
              (interval-inside? interval (get-interval-from-map m)) (map (fn [a] (get-value a maps)) result)
              :else (->> (split-interval interval (get-interval-from-map m))
                      (map (fn [i] (convert i maps))))
              ))
    interval
    maps))

(defn flatten-mixed [lst]
  (mapcat (fn [item]
            (if (seq? (first item))
              (flatten-mixed item)
              [item]))
    lst))

(defn part2 [data]
  (let [l               (parse data)
        seeds           (map read-string (re-seq #"\d+" (first l)))
        seeds-intervals (map (fn [a b]
                               (get-interval-from-seed (list a b))) (take-nth 2 seeds) (take-nth 2 (rest seeds)))]
    (->>
      l
      (create-maps)
      (reduce
        (fn [result maps]
          (println
            (map #(convert % maps) result))
          (flatten-mixed (map #(convert % maps) result)))
        seeds-intervals)
      (map first)
      (apply min)
      )))

(part2 sample1)
(part2 input)
