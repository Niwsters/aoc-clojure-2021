(ns shared.input
  (:require [clojure.string :as str]))

(defn- test-input []
  "
   forward 5
   down 5
   forward 8
   up 3
   down 8
   forward 2
  ")

(defn lines [source]
  (filter not-empty
    (map str/trim
      (str/split-lines source))))

(comment
  (lines (test-input))
)
