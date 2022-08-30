(ns day15.solution
  (:require [clojure.string :as str]
            [shared.util :as util]
            [day15.input]
            [taoensso.tufte :as tufte]
            [clojure.stacktrace :as stacktrace]
            [clojure.data.priority-map :as pm]))

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
        dist (map #(into [% ##Inf]) dist)
        dist (into {} dist)
        dist (assoc dist [0 0] 0)]
    dist))

(defn- distance [dist v]
  (get dist v ##Inf))

(defn- u [dist queue]
  (let [dist (tufte/p :dist (sort-by val dist))]
    (reduce
      (fn [_ [v _]]
        (if (contains? queue v)
          (reduced v)
          nil))
      nil
      dist)))

(defn- queue [dist]
  (into (pm/priority-map) (seq dist)))

(defn- queue-set-weight [queue u weight]
  (assoc queue u weight))

(defn- queue-remove [queue u]
  (dissoc queue u))

(defn- queue-min [queue]
  (peek queue))

(defn- dijkstras [input goal]
  (let [dict      (dict input)
        dist      (distances dict)
        queue     (queue dist)
        neighbors (neighbors input)]
    (loop [queue queue
           dist  dist]
      (let [[u _]       (tufte/p :queue-min (queue-min queue))
            [ux uy]     u
            queue       (tufte/p :queue-remove (queue-remove queue u))]
        (if (or (empty? queue) (= u goal))
          dist
          (let [neighbors   (neighbors ux uy)
                neighbors   (filter #(contains? queue %) neighbors)
                alts        (map #(into [% (+ (get dict %) (distance dist u))]) neighbors)
                [dist queue] (reduce
                              (fn [[dist queue] [v alt]]
                                (if (< alt (distance dist v))
                                  [(assoc dist v alt) (queue-set-weight queue v alt)]
                                  [dist queue]))
                              [dist queue]
                              alts)]
            (recur queue dist)))))))

(defn- solution [input goal]
  (let [width       (width input)
        height      (height input)
        ;goal        [(- width 1) (- height 1)]
        dist        (dijkstras input goal)]
    (distance dist goal)))

(tufte/add-basic-println-handler! {})

(comment
  (stacktrace/print-stack-trace *e)

  (let [dict      (dict test-input)
        dist      (distances dict)
        queue     (queue dist)]
    queue)

  (tufte/profile ; Profile any `p` forms called during body execution
    {} ; Profiling options; we'll use the defaults for now
    (tufte/p :solution (solution day15.input/input [99 99])))
)
