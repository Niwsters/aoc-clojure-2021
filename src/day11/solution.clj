(ns day11.solution
  (:require [clojure.string :as str]
            [clojure.stacktrace :as stacktrace]))

(def test-input
"11111
19991
19191
19991
11111")

(def test-input-2
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

(defn- parse-int [s]
  (Integer/parseInt s))

(defn- coord-str [x y]
  (str x \: y))

(defn- octopus [x y s]
  {:energy (parse-int s) :coord (coord-str x y)})

(defn- unstr-coord [coord-str]
  (->> (str/split coord-str #":")
       (map parse-int)))

(defn- octopi [input]
  (->> (str/split input #"\n")
       (map #(str/split % #""))
       (map-indexed
         (fn [y row]
           (map-indexed
             (fn [x s]
               (octopus x y s))
             row)))
       (flatten)))

(defn odict [octopi]
  (reduce
    (fn [octopi octopus]
      (assoc octopi (:coord octopus) (:energy octopus)))
    {}
    octopi))

(defn- neighboring-coords
  ([x y]
    (->> [(- x 1) (- y 1)
          x       (- y 1)
          (+ x 1) (- y 1)
          (- x 1) y
          ;x       y
          (+ x 1) y
          (- x 1) (+ y 1)
          x       (+ y 1)
          (+ x 1) (+ y 1)]
         (partition 2)
         (map #(coord-str (first %) (second %)))))
  ([coord-str]
   (let [[x y] (unstr-coord coord-str)]
     (neighboring-coords x y))))

(defn- neighboring-energies [odict octopus]
  (->> (:coord octopus)
       (neighboring-coords)
       (map #(get odict %))
       (filter #(not (nil? %)))))

(defn- octopus-inc [octopus]
  (assoc octopus :energy (inc (:energy octopus))))

(defn- octopus-get-flashed [odict octopus]
  (assoc
    octopus
    :energy
    (->> (neighboring-energies odict octopus)
         (filter #(> % 9))
         (count)
         (+ (:energy octopus)))))

(defn- flashed-octopi [octopi]
  (->> (map #(:energy %) octopi)
       (filter #(> % 9))
       (count)))

(defn- octopi-get-flashed [octopi]
  (loop [octopi     octopi
         old-octopi []
         iterations 0]
    (println (flashed-octopi octopi))
    (if (or
          (> iterations 10)
          (= (flashed-octopi octopi)
             (flashed-octopi old-octopi)))
      octopi
      (let [odict         (odict octopi)
            new-octopi    (map #(octopus-get-flashed odict %) octopi)]
        (recur
          new-octopi
          octopi
          (inc iterations))))))

(defn- octopus-flash [octopus]
  (assoc
    octopus
    :energy
    (if (> (:energy octopus) 9)
      0
      (:energy octopus))))

(defn- step [octopi]
  (let [octopi  (map octopus-inc octopi)
        octopi  (octopi-get-flashed octopi)
        octopi  (map octopus-flash octopi)]
    octopi))

(defn- step-n-times [octopi n]
  (loop [octopi octopi
         i      0]
    (if (>= i n)
      octopi
      (recur
        (step octopi)
        (inc i)))))

(defn- max-coords [octopi]
  (let [coords      (->> (map #(:coord %) octopi)
                    (map #(str/split % #":"))
                    (map #(map parse-int %)))
        x-coords    (map #(first %) coords)
        y-coords    (map #(second %) coords)]
    {:x (apply max x-coords)
     :y (apply max y-coords)}))

(defn- render [octopi]
  (let [max-coords    (max-coords octopi)]
    (->> (map #(:energy %) octopi)
         (partition (inc (:y max-coords)))
         (map #(apply str %))
         (map #(str % "\n"))
         (flatten)
         (str/join)
         (str/trim))))

(def test-expected-step-1
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

(def test-expected-step-2
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

(comment
  (unstr-coord "0:1")
  (octopi test-input)

  (= (step (step (octopi test-input-2)))
     (octopi test-expected-step-2))

  (stacktrace/print-stack-trace *e)

  (let [octopi  (octopi test-input-2)]
    (println (render (step-n-times octopi 2))))
  (neighboring-coords 0 0)
)
