(ns day03.part1
  (:require [shared.input :refer [lines]]
            [shared.assert :refer [assert-equal]]
            [day03.input]))

(defn test-input [] 
  "
    00100
    11110
    10110
    10111
    10101
    01111
    00111
    11100
    10000
    11001
    00010
    01010
  ")

(defn- nth-column [input-source n]
  (map (fn [line] (nth line n)) (lines input-source)))

(defn- row-range [input-source]
  (range (count (first (lines input-source)))))

(defn- bit-frequencies [bits]
  (seq (sort-by val (frequencies bits))))

(defn- most-common [bits]
  (first (second (bit-frequencies bits))))

(defn- least-common [bits]
  (first (first (bit-frequencies bits))))

(defn- to-decimal [binary]
  (Integer/parseInt binary 2))

(defn- rate [func input-source]
  (->> input-source
   (row-range)
   (map (partial nth-column input-source))
   (map func)
   (reduce str)
   (to-decimal)))

(defn- gamma-rate [input-source]
  (rate most-common input-source))

(defn- epsilon-rate [input-source]
  (rate least-common input-source))

(defn result [input-source]
  (* (gamma-rate input-source) (epsilon-rate input-source)))

(comment
  (assert-equal 198 (result (test-input)))
  (assert-equal 4138664 (result (day03.input/input)))
)
