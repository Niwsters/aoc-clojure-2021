(ns day12.solution
  (:require [clojure.string :as str]))

(def test-input
"start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn paths [input]
  (let [paths (str/split input #"\n")]
    paths))

(defn- includes? [item coll]
  (not (nil? (some #(= item %) coll))))

(defn- all-lowercase? [s]
  (= s (str/lower-case s)))

(defn- all-uppercase? [s]
  (= s (str/upper-case s)))

(comment
  (all-lowercase? "abC")
)

(defn- end [traversed]
  (last (str/split traversed #"-")))

(defn possibilities [paths traversed]
  (let [location (end traversed)
        poss     (map #(str/split % #"-") paths)
        poss     (filter #(includes? location %) poss)
        poss     (map (fn [split] (filter #(not (= location %)) split)) poss)
        poss     (flatten poss)
        visited  (set (str/split traversed #"-"))
        poss     (filter #(or
                            (not (includes? % visited))
                            (all-uppercase? %)) poss)]
    (println visited)
    poss))

(defn traverse [paths]
  (let [all-traversed ["start-b-A-b"]
        traversed     (first all-traversed)]
    (map #(str traversed "-" %) (possibilities paths traversed))))

(comment
  (paths test-input)
  (let [paths (paths test-input)]
    (traverse paths))

  (some #(= "c" %) ["a" "b"])
  (end "start-A-b")
)
