(ns day03.part2
  (:require [shared.input :refer [lines]]
            [shared.assert :refer [assert-equal]]
            [shared.math :refer [transpose]]
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

(defn- frequencies-equal [freqs]
  (= (second (first freqs)) (second (second freqs))))

(defn- most-common [input n]
  (->> input
       (transpose)
       ((fn [lines] (nth lines n)))
       (frequencies)
       (sort-by val)
       ((fn [freqs]
         (if (frequencies-equal freqs)
           \1
           (first (second freqs)))))))

(defn- least-common [input n]
  (->> input
       (transpose)
       ((fn [lines] (nth lines n)))
       (frequencies)
       (sort-by val)
       ((fn [freqs]
         (if (frequencies-equal freqs)
           \0
           (first (first freqs)))))))

(defn- filter-line [n most-common line]
  (= most-common (nth line n)))

(defn- filter-lines [n most-common lines]
  (filter (partial filter-line n most-common) lines))

(defn- rating
  ([filt input]
   (rating 0 filt input))
  ([n filt input]
    (if (== (count input) 1)
      (first input)
      (rating
        (+ n 1)
        filt
        (filter-lines n (filt input n) input)))))

(defn- to-decimal [binary]
  (Integer/parseInt binary 2))

(defn- oxygen-rating [input]
  (to-decimal (rating most-common input)))

(defn- co2-rating [input]
  (to-decimal (rating least-common input)))

(defn result [input]
  (*
   (oxygen-rating input)
   (co2-rating input)))

(comment
  (result (lines (test-input)))
  (result (lines (day03.input/input)))
)
