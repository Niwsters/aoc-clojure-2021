(ns day05.solution
  (:require [clojure.string :as str]
            [day05.input]
            [taoensso.tufte :as tufte]))

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

(defn- line-dx [line]
  (- (:x (:to line)) (:x (:from line))))

(defn- line-dy [line]
  (- (:y (:to line)) (:y (:from line))))

(defn- sign [x]
  (/ x (abs x)))

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

(defn- init-result []
  {:uniques  #{}
   :overlaps #{}})

(defn- each-coord [result coord]
  (let [uniques  (:uniques result)
        overlaps (:overlaps result)]
    {:overlaps   (if (contains? uniques coord)
                   (conj overlaps coord)
                   overlaps)
     :uniques    (conj uniques coord)}))

(defn- each-line [result line]
  (let [coords    (line-coords line)]
    (reduce each-coord result coords)))

(tufte/add-basic-println-handler! {})

(defn solution [input]
  (let [entries     (entries input)
        lines       (lines entries)]
    (count (:overlaps (reduce each-line (init-result) lines)))))

(comment
  (let [h (line "0,0 -> 5,0")
        v (line "0,0 -> 0,5")
        d (line "5,5 -> 0,0")]
    (line-coords d))

  (tufte/profile ; Profile any `p` forms called during body execution
    {} ; Profiling options; we'll use the defaults for now
    (tufte/p :solution (solution (day05.input/input))))
)
