(ns day06.solution
  (:require [clojure.string :as str]
            [day06.input]))

(defn- parse-int [s]
  (Integer/parseInt s))

(defn- init-state []
  {0 0
   1 0
   2 0
   3 0
   4 0
   5 0
   6 0
   7 0
   8 0})

(defn- each-fish [state fish]
  (assoc state fish (+ 1 (get state fish))))

(defn- test-input [] "3,4,3,1,2")

(defn- state [input]
  (let [fishes  (str/split input #",")
        fishes  (map parse-int fishes)
        state   (reduce each-fish (init-state) fishes)]
    state))

(defn- step [state]
  {0 (get state 1)
   1 (get state 2)
   2 (get state 3)
   3 (get state 4)
   4 (get state 5)
   5 (get state 6)
   6 (+ (get state 0) (get state 7))
   7 (get state 8)
   8 (get state 0)})

(defn- count-fish [state]
  (reduce + (vals state)))

(defn- solution [input iterations]
  (count-fish (reduce (fn [state, _] (step state)) (state input) (range iterations))))

(comment
  (solution (day06.input/input) 256)
)
