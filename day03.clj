(ns advent-of-code-2023.day03
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(def sample1
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def input (slurp "inputs/day03"))

(defn parse [data]
  (->> data
    (str/split-lines)))

(defn re-seq-pos [pattern string] 
  (let [m (re-matcher pattern string)] 
    ((fn step [] 
       (when (. m find) 
         (cons {:start (. m start) :end (. m end) :group (. m group)} 
           (lazy-seq (step))))))))
; (re-seq-pos #"\d+" "467..114..")
; ({:start 0, :end 3, :group "467"} {:start 5, :end 8, :group "114"})


(defn neighbours [[x y]]
  (for [i [-1 0 1]
        j [-1 0 1]]
    [(+ x i) (+ j y)]))
; (neighbours [0 0])
; ([-1 -1] [-1 0] [-1 1] [0 -1] [0 0] [0 1] [1 -1] [1 0] [1 1])

(defn is-symbol? [c]
  (nil? (re-matches #"\d|\." c)))
; (is-symbol? "*")
; true

(defn is-valid? [{:keys [start end y group]} v]
  (let [indices (for [x (range start end)] [y x])
        coords  (mapcat neighbours indices)]
    (->> coords
      (remove (set indices))
      (some #(is-symbol? (str (get-in v % "."))))
      )))
; (is-valid? {:start 0, :end 3, :y 0, :group "467"} (parse sample1))
; true

(defn get-all-by [regex v]
  (->> v
    (map-indexed (fn [y row]
                   (map (fn [n] (assoc n :y y))
                     (re-seq-pos regex row))))
    (apply concat)
    ))
; (get-all-by #"\d+" (parse sample1))
; ({:start 0, :end 3, :group "467", :y 0} {:start 5, :end 8, :group "114", :y 0} {:start 2, :end 4, :group "35", :y 2} {:start 6, :end 9, :group "633", :y 2} {:start 0, :end 3, :group "617", :y 4} {:start 7, :end 9, :group "58", :y 5} {:start 2, :end 5, :group "592", :y 6} {:start 6, :end 9, :group "755", :y 7} {:start 1, :end 4, :group "664", :y 9} {:start 5, :end 8, :group "598", :y 9})

(defn part1 [data]
  (let [v (parse data)]
    (->> v
      (get-all-by #"\d+")
      (filter #(is-valid? % v))
      (map :group)
      (map #(Integer/parseInt %))
      (reduce + 0)
      )))

(part1 sample1)
(part1 input)


(defn gear-score [{:keys [start end y group]} v]
  (let [numbers (get-all-by #"\d+" v)
        coords  (set (neighbours [y start]))]
    (->> numbers
      (filter (fn [{:keys [start end y group]}]
                (->> (set (for [x (range start end)] [y x]))
                  (set/intersection coords)
                  (not-empty))))
      (map :group)
      (map #(Integer/parseInt %))
      ((fn [n] (if (= 2 (count n))
                 (reduce * n)
                 0)))
      )))
; (gear-score {:start 3, :end 4, :group "*", :y 1} (parse sample1))
; 16345

(defn part2 [data]
  (let [v (parse data)]
    (->> v
      (get-all-by #"\*")
      (map #(gear-score % v))
      (reduce + 0))))

(part2 sample1)
(part2 input)



