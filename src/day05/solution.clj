(ns day05.solution
  (:require [clojure.string :as str]
            [day05.input]))

(defn- test-input []
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn- parse-int [s]
  (Integer/parseInt s))

(defn- entries [input]
  (str/split input #"\n")) 

(defn- coords [coords-str]
  (let [[x y]  (map parse-int (str/split coords-str #","))
        coords  { :x x :y y }]
    coords))

(defn- line [entry]
  (let [[from to] (str/split entry #" -> ")
        line { :from (coords from) :to (coords to) }]
    line))

(defn- lines [entries]
  (map line entries))

(defn- line-max-x [line]
  (apply max [(:x (:from line)) (:x (:to line))]))

(defn- line-max-y [line]
  (apply max [(:y (:from line)) (:y (:to line))]))

(defn lines-max-coords [lines]
  (let [max-x (apply max (map line-max-x lines))
        max-y (apply max (map line-max-y lines))]
    { :x max-x :y max-y }))

(defn- draw-row [max-x _]
  (str/join (concat (map (fn [_] ".") (range max-x)) "\n")))

(defn- draw-board [lines]
  (let [max-coords (lines-max-coords lines)]
    (str/join (map #(draw-row (:x max-coords) %) (range (:y max-coords))))))

(defn- line-dx [line]
  (- (:x (:to line)) (:x (:from line))))

(defn- line-dy [line]
  (- (:y (:to line)) (:y (:from line))))

(defn- sign [x]
  (/ x (abs x)))

(defn- line-increments [line]
  (let [dx (line-dx line)
        dy (line-dy line)]
    (if (< (abs dx) (abs dy))
      (range (:y (:from line)) (inc (:y (:to line))) (sign dy))
      (range (:x (:from line)) (inc (:x (:to line))) (sign dx)))))

(defn- line-type [line]
  (let [dx (line-dx line)
        dy (line-dy line)]
    (if (= dx 0)
      :vertical
      (if (= dy 0)
        :horizontal
        :diagonal))))

(defn- line-coords-horizontal [line]
  (let [from    (:from line)
        to      (:to line)
        dx      (line-dx line)
        xs      (range (:x from) (+ (:x to) (sign dx)) (sign dx))
        y       (:y from)
        coords  (map #(str % \: y) xs)]
    coords))

(defn- line-coords-vertical [line]
  (let [from    (:from line)
        to      (:to line)
        dy      (line-dy line)
        ys      (range (:y from) (+ (:y to) (sign dy)) (sign dy))
        x       (:x from)
        coords  (map #(str x \: %) ys)]
    coords))

(defn- line-coords-diagonal [line]
  (let [from    (:from line)
        to      (:to line)
        dx      (line-dx line)
        dy      (line-dy line)
        x-inc  (sign dx)
        y-inc  (sign dy)
        x-range (range (:x from) (+ (:x to) x-inc) x-inc)
        y-range (range (:y from) (+ (:y to) y-inc) y-inc)]
    (map #(str %1 \: %2) x-range y-range)))

(defn- line-coords [line]
  (case (line-type line)
    :horizontal (line-coords-horizontal line)
    :vertical   (line-coords-vertical   line)
    :diagonal   (line-coords-diagonal   line)))

(defn- coords-overlaps [a b] [])

(defn- each-line [result line]
  (let [uniques   (:uniques result)
        overlaps  (:overlaps result)
        coords    (line-coords line)]
    {:uniques    (set (concat uniques coords))
     :overlaps  (set (concat overlaps (coords-overlaps coords uniques)))}))

(comment
  (let [h (line "0,0 -> 5,0")
        v (line "0,0 -> 0,5")
        d (line "5,5 -> 0,0")]
    (line-coords d))

  (map (fn [a b] [a b]) [1 2 3] [4 5 6])

  (let [entries     (entries (day05.input/input))
        lines       (lines entries)
        line        (first lines)]
    (reduce each-line lines))
)
