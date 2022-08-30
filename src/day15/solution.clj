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

(defn- increase [n amount]
  (let [n (+ n amount)
        n (mod n 9)
        n (if (= 0 n) 9 n)]
    n))

(defn- input-block [input amount]
  (->> (str/split input #"\n")
       (map #(str/split % #""))
       (flatten)
       (map util/parse-int)
       (map #(increase % amount))
       (partition (width input))))

(def small-test-input
"1111
1111
1111
1111")

(defn- zero [_] 0)

(defn- map-block [input]
  (let [width (width input)
        height (height input)
        arr (->> (map (fn [_] (map zero (range width))) (range height))
                 (map vec)
                 (vec))
        block (input-block input 0)]
    (reduce
      (fn [arr y]
        (reduce
          (fn [arr x]
            (assoc-in arr [y x] (nth (nth block y) x)))
          arr 
          (range 4)))
      arr
      (range 4))))

(comment
  (input-block small-test-input 4)

  (assoc-in [[0 1] [2 3]] [0 1] 7)
  (map-block small-test-input)
)

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

(defn- limit-n [n]
  (let [n (mod n 9)
        n (if (= 0 n) 9 n)]
    n))

(defn- expand-dict-x [dict y-inc n-inc]
  (let [width (inc (apply max (map first (keys dict))))]
    (reduce-kv
      (fn [dict [x y] n]
        (reduce
          (fn [dict r]
            (assoc dict [(+ x (* r width)) (+ y y-inc)] (limit-n (+ n r n-inc))))
          dict
          (range 5)))
      {}
      dict)))

;          (assoc [(+ x 0)   (+ y y-inc)] (limit-n (+ n 0 n-inc)))
;          (assoc [(+ x 4)   (+ y y-inc)] (limit-n (+ n 1 n-inc)))
;          (assoc [(+ x 8)   (+ y y-inc)] (limit-n (+ n 2 n-inc)))
;          (assoc [(+ x 12)  (+ y y-inc)] (limit-n (+ n 3 n-inc)))
;          (assoc [(+ x 16)  (+ y y-inc)] (limit-n (+ n 4 n-inc)))

(defn- expand-dict [dict]
  (let [height (inc (apply max (map second (keys dict))))]
    (apply merge
      (map
        (fn [r]
          (expand-dict-x dict (* r height) r))
        (range 5)))))

;    (expand-dict-x dict 0 0)
;    (expand-dict-x dict 4 1)
;    (expand-dict-x dict 8 2)
;    (expand-dict-x dict 12 3)
;    (expand-dict-x dict 16 4)

(defn- to-2d-array [dict]
  (let [width       (inc (apply max (map first (keys dict))))
        height      (inc (apply max (map second (keys dict))))
        empty-cave  (vec (repeat height (vec (repeat width 0))))]
    (reduce-kv
      (fn [cave [x y] n]
        (assoc-in cave [y x] n))
      empty-cave
      dict)))

(defn- to-string [array-2d]
  (->> (map str/join array-2d)
       (str/join "\n")))

(defn- expand-input [input]
  (to-string (to-2d-array (expand-dict (dict input)))))

(comment
  (expand-dict (dict test-input))
  (println (expand-input small-test-input))
)

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

(defn- solution [input]
  (let [width       (width input)
        height      (height input)
        goal        [(- width 1) (- height 1)]
        dist        (dijkstras input goal)]
    (distance dist goal)))

(tufte/add-basic-println-handler! {})

(comment
  (stacktrace/print-stack-trace *e)

  (repeat 5 (repeat 5 0))

  (let [dict      (dict test-input)
        dist      (distances dict)
        queue     (queue dist)]
    queue)

  ; part 1
  (tufte/profile ; Profile any `p` forms called during body execution
    {} ; Profiling options; we'll use the defaults for now
    (tufte/p :solution (solution (expand-input day15.input/input))))

  ; part 2
)
