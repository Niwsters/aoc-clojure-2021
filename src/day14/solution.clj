(ns day14.solution
  (:require [clojure.string :as str]
            [day14.input]))

(def test-input
"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn- parse-polymer [s]
  (let [pairs (->> (reduce (fn [polymer c] (conj polymer c)) []s)
                   (partition 2 1)
                   (map #(apply str %))
                   (reduce (fn [polymer pair] (assoc polymer pair (inc (get polymer pair 0)))) {}))
        counts (reduce (fn [counts c] (assoc counts c (inc (get counts c 0)))) {} (str/split s #""))]
    [pairs counts]))

(defn- parse-rules [rules]
  (let [rules (str/split rules #"\n")
        rules (map #(str/split % #" -> ") rules)
        rules (into {} rules)]
    rules))

(defn- parse-input [input]
  (let [[polymer rules] (str/split input #"\n\n")
        [pairs counts]  (parse-polymer polymer)
        rules           (parse-rules rules)]
    [pairs counts rules]))

(defn- apply-rule [pairs new-pairs counts new-counts rule]
  (let [[pair insert] rule
        left          (str (first pair) insert)
        left-amount   (get new-pairs left 0)
        right         (str insert (second pair))
        right-amount  (get new-pairs right 0)
        amount        (get pairs pair 0)
        new-pairs     (if (<= amount 0)
                        new-pairs
                        (-> new-pairs
                            (assoc left (+ left-amount amount))
                            (assoc right (+ right-amount amount))))
        new-counts    (assoc new-counts insert (+ (get new-counts insert 0) amount))]
    [new-pairs new-counts]))

(defn- apply-rules [pairs counts rules]
  (reduce (fn [[new-pairs new-counts] rule] (apply-rule pairs new-pairs counts new-counts rule)) [{} counts] rules))

(defn- step-n [pairs counts rules n]
  (reduce (fn [[pairs counts] _] (apply-rules pairs counts rules)) [pairs counts] (range n)))

(defn- solution [input steps]
  (let [[pairs counts rules]  (parse-input input)
        [pairs counts]        (step-n pairs counts rules steps)
        counts                (sort-by val counts)
        [_ min-count]             (first counts)
        [_ max-count]             (last counts)]
    (- max-count min-count)))

(comment
  (solution day14.input/input 10) ; part 1
  (solution day14.input/input 40) ; part 2

  (let [[pairs counts rules]  (parse-input day14.input/input)
        [pairs counts]        (step-n pairs counts rules 40)
        counts                (sort-by val counts)
        [_ min-count]             (first counts)
        [_ max-count]             (last counts)]
    (- max-count min-count))
)
