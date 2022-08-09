(ns day04.part2
  (:require [clojure.string :as str]
            [shared.math :refer [transpose]]
            [day04.input]
            [taoensso.tufte :as tufte]
            [clojure.stacktrace :as stacktrace]))

(defn test-input []
"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn parse-int [s]
  (Integer/parseInt s))

(defn- numbers [input]
  (->> (str/split input #"\n\n")
       (first)
       (#(str/split % #","))
       (map parse-int)))

(defn- parse-board [board-str]
  (->> (str/split board-str #"\n")
       (map str/trim)
       (map #(str/split % #" "))
       (map #(filter not-empty %))
       (map #(map parse-int %))))

(defn- boards [input]
  (->> (str/split input #"\n\n")
       (drop 1)
       (map parse-board)))

(defn- board-won-horizontal? [board]
  (->> (map str/join board)
       (filter #(= "XXXXX" %))
       (count)
       (< 0)))

(defn- board-won-vertical? [board]
  (board-won-horizontal? (transpose board)))

(defn- board-won? [board]
  (some true? [(board-won-horizontal? board) (board-won-vertical? board)]))

(defn- board-on-draw [board number]
  (let [board (->> (flatten board)
              (replace {number \X})
              (partition 5))]
    (if (board-won? board)
      (reduced {:board board :winning-number number})
      board)))

(defn- solve-board [board numbers]
  (reduce board-on-draw board numbers))

(defn- solve-boards [boards numbers]
  (map #(solve-board % numbers) boards))

(defn- index-of [items item]
  (.indexOf items item))

(defn- rounds-until-won [result numbers]
  (inc (index-of numbers (:winning-number result))))

(defn- enrich-rounds-until-won [result numbers]
  (assoc result :rounds-until-won (rounds-until-won result numbers)))

(defn- sort-results-by-rounds [results]
  (sort-by #(:rounds-until-won %) results))

(defn- score [result]
  (->> (:board result)
       (flatten)
       (filter #(not (= \X %)))
       (reduce +)
       (* (:winning-number result))))

(defn- results [input]
  (let [numbers         (numbers input)
        boards          (boards input)
        results         (solve-boards boards numbers)
        results         (map #(enrich-rounds-until-won % numbers) results)
        results         (sort-results-by-rounds results)]
    results))

(defn part1 [input]
  (score (first (results input))))

(defn part2 [input]
  (score (last (results input))))

(comment
  (stacktrace/print-stack-trace *e)
  (part1 day04.input/input)
  (part2 day04.input/input)
)
