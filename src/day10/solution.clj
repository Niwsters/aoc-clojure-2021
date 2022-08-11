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
      {:status      :ok
       :opens-queue []
       :index       0}
      characters)))

(defn- score [c]
  (get {\) 3
   \] 57
   \} 1197
   \> 25137} c))

(comment
  pairs

  (->> (lines (day10.input/input))
       (map validate)
       (filter #(not (= (:status %) :ok)))
       (map #(:got %))
       (map score)
       (reduce +))
  (validate "[(()[<>])]({[<{<<[]>>(")
  (validate "{([(<{}[<>[]}>{[]{[(<()>")
)
