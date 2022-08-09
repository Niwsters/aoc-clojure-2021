(ns day08.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [day08.input]))

(defn test-input []
"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn- entries [input]
  (str/split input #"\n"))

(defn- count-length [numbers length]
  (count (filter #(== length (count %)) numbers)))

(defn- output [entry]
  (let [[_ output]  (str/split entry #" \| ")
        output      (str/split output #" ")]
    output))

(defn- count-1-4-7-8 [numbers]
  (reduce + (map #(count-length numbers %) [2 4 3 7])))

(defn part1 [input]
  (let [entries (entries input)
        outputs (map output entries)]
    (reduce + (map count-1-4-7-8 outputs))))

(defn- find-by [pred items]
  (reduce
    (fn [_ item]
      (if (pred item)
        (reduced item)
        _))
    items))

(defn- numbers [entry]
  (let [numbers (str/split entry #" ")
        numbers (filter #(not (= "|" %)) numbers)]
    numbers))

(defn- identify-by-length [numbers length]
  (find-by #(= length (count %)) numbers))

(defn- one [numbers]
  (identify-by-length numbers 2))

(defn- four [numbers]
  (identify-by-length numbers 4))

(defn- seven [numbers]
  (identify-by-length numbers 3))

(defn- eight [numbers]
  (identify-by-length numbers 7))

(comment
  [0 6  ; 0 has count 6 and diff from 3 (2) and not yet found
   2 5  ; 2 has count 5 and diff from 6 (1)
   3 5  ; 3 has count 5 and no diff from 1 (0)
   5 5  ; 5 has count 5 and no diff from 6 (1)
   6 6  ; 6 has count 6 and diff from 1 (0)
   9 6] ; 9 has count 6 and no diff from 3 (2)
)

(defn- id-1-4-7-8 [numbers]
  {:one    (one numbers)
   :four   (four numbers)
   :seven  (seven numbers)
   :eight  (eight numbers)
   :numbers numbers})

(defn- diff? [a b]
  (let [[shortest longest]  (map set (sort-by #(count %) [a b]))]
    (not (set/subset? shortest longest))))

(defn- identified? [identified number]
  (contains? (set (vals identified)) number))

(defn- id-n [identified comp-num should-diff has-count num-key]
  (let [comp-num   (comp-num identified)
        result (find-by
                #(and
                   (= (count %) has-count)
                   (if should-diff (diff? comp-num %) (not (diff? comp-num %)))
                   (not (identified? identified %)))
                (:numbers identified))]
    (assoc identified num-key result)))

(defn- id-0 [identified]
  (id-n identified :three true 6 :zero))

(defn- id-2 [identified]
  (id-n identified :six true 5 :two))

(defn- id-3 [identified]
  (id-n identified :one false 5 :three))

(defn- id-5 [identified]
  (id-n identified :six false 5 :five))

(defn- id-6 [identified]
  (id-n identified :one true 6 :six))

(defn- id-9 [identified]
  (id-n identified :three false 6 :nine))

(defn- identified [numbers]
  (->> (id-1-4-7-8 numbers)
       (id-3)
       (id-6)
       (id-2)
       (id-5)
       (id-0)
       (id-9)))

(defn- test-entry []
"acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(comment
  (part1 (day08.input/input))

  (contains? (set "abc") \a)
  (diff? "abc" "ab")
  (diff? "abcd" "abe")

  (diff? "fbcad" "ab")
  (diff? "cdfbe" "ab")

  (set/subset? (set "abc") (set "ab"))

  (#(and (= (count %) 5) (not (diff? "ab" %))) "fbcad")
  (#(and (= (count %) 5) (not (diff? "ab" %))) "cdfbe")

  (let [entry   (first (entries (test-entry)))
        numbers (numbers entry)]
    (dissoc (identified numbers) :numbers))

  (let [entries (entries (test-input))
        outputs (map output entries)
        sevens  (map #(seven (numbers %)) entries)])
)
