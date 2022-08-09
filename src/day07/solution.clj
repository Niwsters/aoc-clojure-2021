(ns day07.solution
  (:require [clojure.string :as str]
            [day07.input]))

(defn- test-input []
  "16,1,2,0,4,2,7,1,2,14")

(defn- get-crab [crabs dist]
  (let [crab (get crabs dist)]
    (if (nil? crab)
      0
      crab)))

(defn- parse-int [s]
  (Integer/parseInt s))

(defn- each-number [crabs dist]
  (assoc crabs dist (+ 1 (get-crab crabs dist))))

(defn- crabs [input]
  (let [dists (str/split input #",")
        dists (map parse-int dists)
        crabs (reduce each-number {} dists)]
    crabs))

(defn- distance [crab-coord goal]
  (reduce + (range 1 (inc (abs (- crab-coord goal))))))

(defn- crab-fuel-cost [goal crab]
  (let [[crab-coord crab-count] crab]
    (* (distance crab-coord goal) crab-count)))

(defn- total-fuel-cost [crabs coord]
  (reduce + (map #(crab-fuel-cost coord %) crabs)))

(defn- min-distance [crabs]
  (apply min (keys crabs)))

(defn- max-distance [crabs]
  (apply max (keys crabs)))

(defn- lesser-fuel-cost [crabs min-cost coord]
  (let [fuel-cost (total-fuel-cost crabs coord)]
    (if (< fuel-cost min-cost)
      fuel-cost
      min-cost)))

(defn- min-fuel-cost [crabs]
  (reduce
    #(lesser-fuel-cost crabs %1 %2)
    ##Inf
    (range (min-distance crabs) (max-distance crabs))))

(comment
  (max-distance (crabs (test-input)))
  (min-fuel-cost (crabs (day07.input/input)))
  (total-fuel-cost (crabs (test-input)) 2)
)
