(ns shared.util)

(defn parse-int [s]
  (Integer/parseInt s))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn- neighbors-coords [x y diagonals?]
  (if diagonals?

    [(- x 1) (- y 1)
     x       (- y 1)
     (+ x 1) (- y 1)
     (- x 1) y
     (+ x 1) y
     (- x 1) (+ y 1)
     x       (+ y 1)
     (+ x 1) (+ y 1)]

    [(- x 1) y
     (+ x 1) y
     x       (- y 1)
     x       (+ y 1)]))

(defn neighbors
  ([x y width height diagonals?]
   (let
     [coords (neighbors-coords x y diagonals?)
      coords (partition 2 coords)
      coords (filter
               (fn [coord]
                 (and
                   (< (first coord) width)
                   (< (second coord) height)
                   (every? #(>= % 0) coord)))
               coords)]
     coords))
  ([width height diagonals?]
   (fn [x y] (neighbors x y width height diagonals?))))

(defn includes? [item coll]
  (not (nil? (some #(= item %) coll))))
