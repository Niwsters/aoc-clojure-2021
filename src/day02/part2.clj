(ns day02.part2 
  (:require [clojure.string :as str]
            [day02.input :as day02]
            [shared.assert :refer [assert-equal]]
            [shared.input :refer [lines]]))

(defn init-position [] {:depth 0 :horizontal 0 :aim 0})

(defn horizontal [position]
  (get position :horizontal))

(defn depth [position]
  (get position :depth))

(defn aim [position]
  (get position :aim))

(defn forward [position amount]
  (assoc position
         :depth (+ (depth position) (* amount (aim position)))
         :horizontal (+ (horizontal position) amount)))

(defn down [position amount]
  (assoc position :aim (+ (aim position) amount)))

(defn up [position amount]
  (assoc position :aim (- (aim position) amount)))

(defn instruction [line]
  (let [split (str/split line #" ")]
    { :command (first split) :amount (Integer/parseInt (second split)) }))

(defn command [instruction]
  (get instruction :command))

(defn amount [instruction]
  (get instruction :amount))

(defn func [instruction]
  (case (command instruction)
    "forward" forward
    "down" down
    "up" up))

(defn parse [position instruction]
  ((func instruction) position (amount instruction)))

(def test-input
  "
   forward 5
   down 5
   forward 8
   up 3
   down 8
   forward 2
  ")

(defn input [source]
  (map instruction (lines source)))

(defn result [source]
  (let [position (reduce parse (init-position) (input source))]
    (* (horizontal position) (depth position))))

(assert-equal 900 (result test-input))
(assert-equal 1942068080 (result day02/input))

(comment
  (result test-input)
  (result day02/input)
)
