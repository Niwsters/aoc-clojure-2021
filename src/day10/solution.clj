(ns day10.solution
  (:require [clojure.string :as str]
            [day10.input]))

(defn test-input []
"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(defn lines [input]
  (str/split input #"\n"))

(defn- test-line [] "{([(<{}[<>[]}>{[]{[(<()>")

(def opens (set "([{<"))
(def closed (set ")]}>"))
(def pairs (reduce #(assoc %1 (first %2) (second %2)) {} (map #(into [%1 %2]) opens closed)))

(defn- open? [c] (contains? opens c))
(defn- closed? [c] (contains? closed c))

(defn- characters [line]
  (->> (vec line)))

(defn- add-open [result c]
  (assoc result 
         :opens-queue (conj (:opens-queue result) c)))

(defn- expected-closed [result]
  (get pairs (last (:opens-queue result))))

(defn- remove-last-open [result]
  (assoc result :opens-queue (vec (butlast (:opens-queue result)))))

(defn- unexpected-closed [result c]
  (reduced {:expected (expected-closed result) :got c :index (:index result)}))

(defn- next-index [result]
  (if (reduced? result)
    result
    (assoc result :index (inc (:index result)))))

(defn- validate [line]
  (let [characters (characters line)]
    (reduce
      (fn [result, c]
        (next-index
          (if (open? c)
            (add-open result c)
            (if (= c (expected-closed result))
              (remove-last-open result)
              (unexpected-closed result c)))))
      {:status      :incomplete
       :opens-queue []
       :index       0}
      characters)))

(defn- score [c]
  (get {\) 3
   \] 57
   \} 1197
   \> 25137} c))

(defn- part1 [input]
  (->> (lines input)
       (map validate)
       (filter #(not (= (:status %) :incomplete)))
       (map #(:got %))
       (map score)
       (reduce +)))

(defn- incomplete? [result]
  (= (:status result) :incomplete))

(defn- complete [result]
  (map #(get pairs %) (reverse (:opens-queue result))))

(defn- autocomp-value [c]
  (get
    {\) 1
     \] 2
     \} 3
     \> 4}
    c))

(defn- score-autocomplete [characters]
  (reduce
    (fn [score c]
      (+ (* score 5) (autocomp-value c)))
    0
    characters))

(defn- pick-autocomplete-score [scores]
  (println (int (Math/floor (/ (count scores) 2))))
  (nth (sort scores) (int (Math/floor (/ (count scores) 2)))))

(defn- part2 [input]
  (->> (lines input)
       (map validate)
       (filter incomplete?)
       (map complete)
       (map score-autocomplete)
       (sort)
       (pick-autocomplete-score)))

(comment
  pairs

  (part2 (day10.input/input))

  (->> (validate "[({(<(())[]>[[{[]{<()<>>")
       (complete)
       (score-autocomplete))

  (validate "[(()[<>])]({[<{<<[]>>(")
  (validate "{([(<{}[<>[]}>{[]{[(<()>")
)
