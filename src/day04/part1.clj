(ns day04.part1
  (:require [clojure.string :as str]
            [shared.math :refer [transpose]]
            [day04.input]
            [taoensso.tufte :as tufte]))

(def test-input
"
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

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
 2  0 12  3  7
")

(defn- parse-board [board-str]
  (let [split (str/split board-str #"\n")
        trim (map str/trim split)
        cells (map #(str/split % #" +") trim)]
    cells))

(defn test-board []
  (parse-board "22 13 17 11  0
                 8  2 23  4 24
                21  9 14 16  7
                 6 10  3 18  5
                 1 12 20 15 19"))

(defn- board-on-draw [board drawn-number]
  (->> (flatten board)
       (map #(if (= % drawn-number) "X" %))
       (partition 5)))

(defn- score [board]
  (->> (flatten board)
       (filter #(not (= "X" %)))
       (map #(Integer/parseInt %))
       (reduce +)))

(def test-winning-board
  [["X" "X" "X" "X" "X"]
   ["1" "2" "3" "4" "5"]
   ["3" "X" "6" "7" "8"]])

(defn- row-won? [row]
  (reduce
    (fn [won? cell]
      (if
        (not (= "X" cell))
        (reduced false)
        won?))
    true
    row))

(defn- board-won? [board]
  (>
    (count
      (filter
        row-won? 
        board))
    0))

(defn- board-score-horizontal [board]
  (if (board-won? board)
    (score board)
    0))

(comment
  (board-score-horizontal test-winning-board)
)

(defn- board-score-vertical [board]
  (board-score-horizontal (transpose board)))

(defn- board-score [board]
  (+ (board-score-horizontal board) (board-score-vertical board)))

(defn- boards-score [boards]
  (let [score (first (filter #(> % 0) (map board-score boards)))]
    (if (nil? score)
      0
      score)))

(defn- boards-on-draw [boards drawn-number]
  (map #(board-on-draw % drawn-number) boards))

(defn solution [input]
  (let [split (str/split input #"\n\n")
        draws (str/split (str/trim (first split)) #",")
        boards (map parse-board (drop 1 split))]
    (reduce
      (fn [boards draw]
        (let [boards (boards-on-draw boards draw)
              score  (boards-score boards)]
          (if (> score 0)
            (reduced (* score (Integer/parseInt draw)))
            (boards-on-draw boards draw))))
      boards
      draws)))

(defn part2 [input]
  (let [split (str/split input #"\n\n")
        draws (str/split (str/trim (first split)) #",")
        boards (map parse-board (drop 1 split))]
    (reduce
      (fn [boards draw]
        (let [boards (boards-on-draw boards draw)
              score  (boards-score boards)]
          (println boards)
          (if (<= (count boards) 1)
            (if (> score 0)
              (reduced (* (boards-score boards) (Integer/parseInt draw)))
              (boards-on-draw boards draw))
            (boards-on-draw (filter #(not (board-won? %)) boards) draw))))
      boards
      draws)))

(tufte/add-basic-println-handler! {})

(comment
  (part2 day04.input/input)
  (tufte/profile
    {}
    (tufte/p :part2 (part2 day04.input/input)))
)
