(ns day09.part1
  (:require [clojure.string :as str]
            [day09.input]))

(defn test-input []
"2199943210
3987894921
9856789892
8767896789
9899965678")

(defn- parse-int [s]
  (Integer/parseInt s))

(defn- heightmap [input]
  (->> (str/split input #"\n")
       (map #(str/split % #""))
       (map #(map parse-int %))
       (map vec)
       (vec)))

(defn- get-point [heightmap x y]
  (get (get heightmap y) x))

(defn- heightmap-list [heightmap]
  (flatten
    (map-indexed
        (fn [y row]
          (map-indexed
            (fn [x height]
              { :x x :y y :height height })
            row))
        heightmap)))

(defn- coord-str [x y]
  (str x ":" y))

(defn- hdict [hlist]
  (reduce
      (fn [hdict cell]
        (assoc hdict (coord-str (:x cell) (:y cell)) (:height cell)))
      {}
      hlist))

(defn- surrounding-coords [x y]
  (->> [[(- x 1) y]
        [(+ x 1) y]
        [x (- y 1)]
        [x (+ y 1)]]
       (map #(coord-str (first %) (second %)))))

(defn- surrounding-points
  ([hdict x y]
    (filter #(not (nil? %)) (map #(get hdict %) (surrounding-coords x y))))
  ([hdict cell]
    (surrounding-points hdict (:x cell) (:y cell))))

(defn- low-points [hlist hdict]
  (filter
      (fn [cell]
        (every? #(< (:height cell) %) (surrounding-points hdict (:x cell) (:y cell))))
      hlist))

(defn part1 [input]
  (let [heightmap   (heightmap input)
        hlist       (heightmap-list heightmap)
        hdict       (hdict hlist)
        low-points  (low-points hlist hdict)]
    (->> (map #(:height %) low-points)
         (map #(+ 1 %))
         (reduce +))))

(comment
  (surrounding-coords 1 0)

  (part1 (day09.input/input))

  (let [heightmap   (heightmap (test-input))
        hlist       (heightmap-list heightmap)
        hdict       (hdict hlist)
        low-points  (low-points hlist hdict)]
    (->> (map #(surrounding-points hdict %) low-points)))

  (heightmap (day09.input/input))
  (get (heightmap (test-input)) 1)
  (get-point (heightmap (test-input)) 0 0)
  (get (get [[1 2] [3 4]] 0) 0)
)
