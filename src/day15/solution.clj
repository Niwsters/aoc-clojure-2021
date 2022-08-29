(ns day15.solution
  (:require [clojure.string :as str]
            [shared.util :as util]
            [day15.input]))

(def test-input
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defn- width [input]
  (->> (str/split input #"\n")
       (first)
       (count)))

(defn- height [input]
  (->> (str/split input #"\n")
       (count)))

(defn- neighbors [input]
  (let [width  (width input)
        height (height input)]
    (util/neighbors width height false)))

(comment
  (width test-input)
  (height test-input)
  ((neighbors test-input) 9 9)
)

(defn- dict [input]
  (->> (str/split input #"\n")
       (map #(str/split % #""))
       (map #(map util/parse-int %))
       (map-indexed
         (fn [y row]
           (map-indexed
             (fn [x n]
               [[x y] n])
             row)))
       (apply concat)
       (into {})))

(defn- distances [dict]
  (let [dist (keys dict)
        dist (map (fn [v] [v ##Inf]) dist)
        dist (into {} dist)
        dist (assoc dist [0 0] 0)]
    dist))

(defn- dijkstras [input]
  (let [dict      (dict input)
        prev      {}
        queue     (keys dict)
        dist      (distances dict)
        neighbors (neighbors input)]
    (loop [queue queue
           dist  dist
           prev  prev]
      (if (empty? queue)
        [dist prev]
        (let [queue       (sort-by #(get dist %) queue)
              u           (first queue)
              [ux uy]     u
              queue       (drop 1 queue)
              neighbors   (neighbors ux uy)
              neighbors   (filter #(util/includes? % queue) neighbors)
              alts        (map #(into [% (+ (get dict %) (get dist u))]) neighbors)
              [dist prev] (reduce
                            (fn [[dist prev] [v alt]]
                              (if (< alt (get dist v))
                                [(assoc dist v alt) (assoc prev v u)]
                                [dist prev]))
                            [dist prev]
                            alts)]
          (recur queue dist prev))))))

(comment
  (let [input       day15.input/input 
        [dist _]    (dijkstras input)
        width       (width input)
        height      (height input)]
    (get dist [(- width 1) (- height 1)]))
)
