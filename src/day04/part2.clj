(ns day04.part2
  (:require [clojure.string :as str]
            [shared.math :refer [transpose]]
            [day04.input]
            [taoensso.tufte :as tufte]))

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
      (reduced board)
      (board))))

(defn- solve-board [board numbers]
  (reduce board-on-draw board numbers))

(defn- boards [input]
  (->> (str/split input #"\n\n")
       (drop 1)
       (map parse-board)))

(defn- test-winning-board []
  [[\X \X \X \X \X]])

(defn- test-winning-board-v []
  (transpose (test-winning-board)))

(comment
  (solve-board (first (boards (test-input))) (numbers (test-input)))
)
