(ns day11.solution
  (:require [clojure.string :as str]
            [clojure.stacktrace :as stacktrace]
            [shared.util :as util]))

(def input
"5433566276
6376253438
8458636316
6253254525
7211137138
1411526532
5788761424
8677841514
1622331631
5876712227")

(def test-input
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def expected-step-1
"6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637")

(def expected-step-2
"8807476555
5089087054
8597889608
8485769600
8700908800
6600088989
6800005943
0000007456
9000000876
8700006848")

(def small-test-input
"11111
19991
19191
19991
11111")

(defn- size [octopi]
  (let [all-coords (keys octopi)
      all-coords (sort all-coords)
      highest    (last all-coords)
      split      (str/split highest #":")
      split      (map util/parse-int split)
      split      (map inc split)]
    split))

(defn- width [octopi]
  (first (size octopi)))

(defn- height [octopi]
  (second (size octopi)))

(defn- neighbors
  ([x y width height]
   (let
     [coords [(- x 1) (- y 1)
              x       (- y 1)
              (+ x 1) (- y 1)
              (- x 1) y
              (+ x 1) y
              (- x 1) (+ y 1)
              x       (+ y 1)
              (+ x 1) (+ y 1)]
      coords (partition 2 coords)
      coords (filter
               (fn [coord]
                 (and
                   (< (first coord) width)
                   (< (second coord) height)
                   (every? #(>= % 0) coord)))
               coords)]
     coords))
  ([coords width height]
   (let [[x y]      (str/split coords #":")
         x          (util/parse-int x)
         y          (util/parse-int y)
         neighbors  (neighbors x y width height)
         neighbors  (map #(str (first %) \: (second %)) neighbors)]
     neighbors)))

(comment
  (neighbors "4:4" 5 5)
)

(defn- map-octopi [func octopi]
  (map-indexed
    (fn [y row]
      (map-indexed
        (fn [x octopus]
          (func x y octopus))
        row))
    octopi))

(defn- dict [octopi]
  (let [octopi (map-octopi #(into [(str %1 \: %2) %3]) octopi)
        octopi (apply concat octopi)
        octopi (reduce #(assoc %1 (first %2) (second %2)) {} octopi)]
    octopi))

(defn- flash-neighbor [octopi coords]
  (if (and
        (> (get octopi coords) -1)
        (contains? octopi coords))
    (assoc octopi coords (inc (get octopi coords)))
    octopi))

(defn- flash-octopus [octopi coords energy]
  (if (> energy 9)
    (let [octopi    (assoc octopi coords -1)
          neighbors (neighbors coords (width octopi) (height octopi))
          octopi    (reduce flash-neighbor octopi neighbors)]
      octopi)
    octopi))

(defn- flash-once [octopi]
  (let [octopi (reduce-kv flash-octopus octopi octopi)]
    octopi))

(defn- amount-flashed [octopi]
  (->> (vals octopi)
       (filter #(= -1 %))
       (count)))

(defn- flash [octopi]
  (loop [octopi octopi
         prev-flashed -1]
    (if (= (amount-flashed octopi) prev-flashed)
      octopi
      (recur (flash-once octopi) (amount-flashed octopi)))))

(defn- increment [octopi]
  (reduce-kv
    (fn [octopi coords energy]
      (assoc octopi coords (inc energy)))
    octopi
    octopi))

(defn- reset [octopi]
  (reduce-kv
    (fn [octopi coords energy]
      (if (= energy -1)
        (assoc octopi coords 0)
        octopi))
    octopi
    octopi))

(defn- step
  ([octopi flash-count]
   (let
     [octopi (increment octopi) ; first, the energy level of each octopus increases by 1
      octopi (flash octopi) ; then, flash octopus with energy level greater than 9 flashes
      flash-count (+ flash-count (amount-flashed octopi))
      octopi (reset octopi) ; finally, any octopus that flashed resets to 0
      ]
     [octopi flash-count]))
  ([octopi]
   (step octopi 0)))

(defn- step-n [octopi n]
  (reduce
    (fn [[octopi flash-count] _]
      (step octopi flash-count))
    [octopi 0]
    (range n)))

(defn- octopi [input]
  (->> (str/split input #"\n")
       (map #(str/split % #""))
       (map #(map util/parse-int %))
       (dict)))

(defn- part1 [input]
  (let
    [octopi (octopi input)
     [octopi flash-count] (step-n octopi 100)]
    flash-count))

(defn- all-zeroes? [octopi]
  (->> (vals octopi)
       (every? #(= 0 %))))

(defn- part2 [input]
  (loop [octopi     (octopi input)
         iterations 0]
    (if (all-zeroes? octopi)
      iterations 
      (recur (first (step octopi)) (inc iterations)))))

(defn- render [octopi]
  (->> (sort-by #(str (reverse (key %))) octopi)
       (vals)
       (partition (width octopi))
       (map #(apply str %))))

(comment
  (stacktrace/print-stack-trace *e)
  (sort-by #(str (reverse (key %))) (octopi small-test-input))

  (part1 input)
  (part2 input)
  (render (step (octopi small-test-input)))
  (render (octopi test-input))
  (render (step (step (octopi test-input))))
)
