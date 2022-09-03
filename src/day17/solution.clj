(ns day17.solution
  (:require [clojure.string :as str]
            [shared.util :as util]))

(def test-input "target area: x=20..30, y=-10..-5")
(def input "target area: x=195..238, y=-93..-67")

(defn- area [input]
  (let [[_ area] (str/split input #": ")
        [x y]    (str/split area #", ")
        [min-x
         max-x]  (map util/parse-int (str/split (subs x 2) #"\.\."))
        [min-y
         max-y]  (map util/parse-int (str/split (subs y 2) #"\.\."))]
    { :min-x min-x :max-x max-x :min-y min-y :max-y max-y }))

(defn- probe-in-area? [area probe]
  (let [x (:x probe)
        y (:y probe)]
    (and
      (>= x (:min-x area))
      (<= x (:max-x area))
      (>= y (:min-y area))
      (<= y (:max-y area)))))

(defn- probe-missed? [area probe]
  (let [y     (:y probe)
        min-y (:min-y area)]
    (< y min-y)
    ))

(defn- probe [x-velocity y-velocity]
  {:x          0
   :y          0
   :x-velocity x-velocity
   :y-velocity y-velocity
   :init-velocity [x-velocity y-velocity]
   :max-y      0})

(defn- step [probe]
  (let [x          (:x probe)
        y          (:y probe)
        x-velocity (:x-velocity probe)
        y-velocity (:y-velocity probe)
        max-y      (:max-y probe)]
    (-> probe
        (assoc :x (+ x x-velocity))
        (assoc :y (+ y y-velocity))
        (assoc :x-velocity (case x-velocity
                             0 0
                             (dec x-velocity)))
        (assoc :y-velocity (dec y-velocity))
        (assoc :max-y (max max-y y))
       )))

(defn- launch [probe area]
  (loop [probe probe]
    (cond (probe-in-area? area probe) (assoc probe :hit? true)
          (probe-missed? area probe) (assoc probe :hit? false)
          :else (recur (step probe)))))

(defn- max-probe [a b]
  (if (> (:max-y a) (:max-y b))
    a
    b))

(defn- launch-all [probes area]
  (loop [probes (pop (util/queue probes)) result (peek probes)]
    (if (empty? probes)
      result
      (let [probe  (peek probes)
            probes (pop probes)
            probe  (launch probe area)]
        (if (:hit? probe)
          (recur probes (max-probe probe result))
          (recur probes result))))))

(defn- test-probes [min-range max-range]
  (for [x (range min-range max-range)
        y (range min-range max-range)]
    (probe x y)))

(comment
  (area test-input)
  (probe-in-area? (area test-input) {:x 25 :y -6})
  (let [area  (area input)
        probes (test-probes 0 100)]
    (launch-all probes area))
)
