(ns day05.solution
  (:require [clojure.string :as str]))

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

(comment
  (let [entries     (entries (test-input))
        lines       (lines entries)]
    (draw-board lines))
)
