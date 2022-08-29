(ns day13.solution
  (:require [clojure.string :as str]
            [shared.util :as util]
            [day13.input]))

(def test-input
"6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn- parse-input [input]
  (let [[points folds] (str/split input #"\n\n")]
    [points folds]))

(defn- points [input]
  (let [points (first (parse-input input))
        points (str/split points #"\n")
        points (map #(str/split % #",") points)
        points (map #(map util/parse-int %) points)
        points (set points)]
    points))

(defn- folds [input]
  (let [folds (second (parse-input input))
        folds (str/split folds #"\n")
        folds (map #(last (str/split % #" ")) folds)
        folds (map #(str/split % #"=") folds)
        folds (map (fn [[direction amount]] [direction (util/parse-int amount)]) folds)]
    folds))

(comment
  (folds test-input)
)

(defn- fold-calc [coord amount]
  (- coord (* (- coord amount) 2)))

(defn- fold-x [points amount]
  (map (fn [[x y]]
              (if (< x amount)
                [x y]
                [(fold-calc x amount) y]))
            points))

(defn- fold-y [points amount]
  (map (fn [[x y]]
              (if (< y amount)
                [x y]
                [x (fold-calc y amount)]))
            points))

(defn- fold [points direction amount]
  (set
    (if (= direction "x")
      (fold-x points amount)
      (fold-y points amount))))

(defn- apply-folds [points folds]
  (reduce
    (fn [points [direction amount]]
      (fold points direction amount))
    points
    folds))

(defn- max-x [points]
  (apply max (map #(first %) points)))

(defn- max-y [points]
  (apply max (map #(second %) points)))

(defn- max-coords [points]
  [(max-x points)
   (max-y points)])

(defn- draw [points]
  (let [[max-x max-y] (max-coords points)]
    (reduce
      (fn [board y]
        (str
          board
          (reduce
            (fn [row x]
              (if (contains? points [x y])
                (str row "#")
                (str row ".")))
            ""
            (range (inc max-x)))
          "\n"))
      ""
      (range (inc max-y)))))

(comment
  (fold [[0 0] [3 3] [8 8]] \y 5)

  (let [input  day13.input/input
        points (points input)
        folds  (folds input)
        [direction amount] (first folds)
        part2  (count (fold points direction amount))
        part1  (draw (apply-folds points folds))]
    (println part1)
    (println part2))
)
