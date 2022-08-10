(ns day09.part2
  (:require [clojure.string :as str]
            [day09.input]
            [clojure.stacktrace :as stacktrace]))

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

(defn- coord-str
  ([x y]  (str x ":" y))
  ([cell] (coord-str (:x cell) (:y cell))))

(defn- hdict [hlist]
  (reduce
      (fn [hdict cell]
        (assoc hdict (coord-str (:x cell) (:y cell)) cell))
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
        (every? #(< (:height cell) (:height %)) (surrounding-points hdict cell)))
      hlist))

(defn part1 [input]
  (let [heightmap   (heightmap input)
        hlist       (heightmap-list heightmap)
        hdict       (hdict hlist)
        low-points  (low-points hlist hdict)]
    (->> (map #(:height %) low-points)
         (map #(+ 1 %))
         (reduce +))))

; CRITERIA FOR BASIN POINT:
; - Not 9
(defn- explore-basin  [hdict low-point-coords]
  (let [low-point     (get hdict low-point-coords)
        neighbors     (filter #(< (:height %) 9) (surrounding-points hdict low-point))]
    (loop [queue          (set (map coord-str neighbors))
           explored       #{low-point-coords}
           current-point  low-point-coords]
      (if (= (count queue) 0)
        explored
        (let [next-point  (first queue)
              neighbors   (surrounding-points hdict (get hdict current-point))
              queue       (disj queue next-point)
              queue       (set (concat queue (map coord-str neighbors)))
              queue       (set (filter #(not (contains? explored %)) queue))
              queue       (set (filter #(< (:height (get hdict %)) 9) queue))
              explored    (conj explored current-point)]
          (recur
            queue
            explored
            next-point))))))

(defn- find-basin-points [low-points-coords hdict]
  (map #(explore-basin hdict %) low-points-coords))

(comment
  (surrounding-coords 1 0)

  (= {:a #{1 2} :b #{3 4}} {:a #{1 2} :b #{3 4}})

  (first #{2 1 3})

  (part1 (day09.input/input))

  (stacktrace/print-stack-trace *e)

  (let [heightmap         (heightmap (day09.input/input))
        hlist             (heightmap-list heightmap)
        hdict             (hdict hlist)
        low-points        (low-points hlist hdict)
        low-points-coords (set (map #(coord-str (:x %) (:y %)) low-points))
        basin-points      (find-basin-points low-points-coords hdict)]
    (reduce * (take-last 3 (sort (map #(+ 1 %) (map count basin-points))))))

  (heightmap (day09.input/input))
  (get (heightmap (test-input)) 1)
  (get-point (heightmap (test-input)) 0 0)
  (get (get [[1 2] [3 4]] 0) 0)
)
