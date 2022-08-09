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

(defn- entry-to-line [entry]
  (let [[from to] (str/split entry #" -> ")
        line { :from (coords from) :to (coords to) }]
    line))

(defn- lines [entries]
  (map entry-to-line entries))

(defn- max-generic-coord [entry which]
  (->> (str/split entry #" -> ")
       (map #(str/split % #","))
       (map which)
       (map parse-int)
       (sort-by <)
       (last)))

(defn- max-x-coord [entry]
  (max-generic-coord entry first))

(defn- max-y-coord [entry]
  (max-generic-coord entry second))

(defn max-x [entries]
  (apply max (map max-x-coord entries)))

(defn- max-y [entries]
  (apply max (map max-y-coord entries)))

(defn- max-coords [entries]
  { :x (max-x entries) :y (max-y entries) })

(comment
  (max-x-coord "0,0 -> 4,4")
  (let [entries   (entries (test-input))
        lines     (lines entries)]
    (max-coords entries))
)
