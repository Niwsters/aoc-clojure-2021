(ns day12.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-input
"start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def test-input-2
"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def test-input-3
"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(def input
"QR-da
QR-end
QR-al
start-op
zh-iw
zh-start
da-PF
op-bj
iw-QR
end-HR
bj-PF
da-LY
op-PF
bj-iw
end-da
bj-zh
HR-iw
zh-op
zh-PF
HR-bj
start-PF
HR-da
QR-bj")

(defn- paths [input]
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

(defn- pb-dict [paths]
  (let [paths (map #(str/split % #"-") paths)]
    (reduce
      (fn [dict path]
        (let [a (first path)
              b (second path)
              pb-a (get dict a)
              pb-b (get dict b)]
        (-> dict
            (assoc a (set/union pb-a #{b}))
            (assoc b (set/union pb-b #{a})))))
      {}
      paths)))

(defn- possibilities [paths traversed]
  (let [location (end traversed)
        poss     (map #(str/split % #"-") paths)
        poss     (filter #(includes? location %) poss)
        poss     (map (fn [split] (filter #(not (= location %)) split)) poss)
        poss     (flatten poss)
        visited  (set (str/split traversed #"-"))
        poss     (filter #(or
                            (not (includes? % visited))
                            (all-uppercase? %)) poss)]
    poss))

(defn- possibilities-2 [paths traversed]
  (let [pbs       (get paths (last traversed))
        visited   (set traversed)
        uppercase (set (filter all-uppercase? pbs))
        pbs       (set/difference pbs visited)
        pbs       (set/union pbs uppercase)]
    pbs))

(defn- traverse [paths traversed]
  (map #(conj traversed %) (possibilities-2 paths traversed)))

(defn- traverse-step [paths all-traversed queue]
  (let [new-traversed (traverse paths (first queue))
        new-traversed (set new-traversed)
        all-traversed (set/union all-traversed new-traversed)
        queue         (drop 1 queue)
        queue         (concat queue new-traversed)
        queue         (filter #(not (= "end" (last %))) queue)]
    [all-traversed queue]))

(defn- traverse-all [paths]
  (let []
    (loop [all-traversed (set [["start"]])
           queue         [["start"]]]
      (if (> (count queue) 0)
        (let [[all-traversed queue] (traverse-step paths all-traversed queue)]
          (recur all-traversed queue))
        all-traversed))))

(defn- count-ends [all-traversed]
  (count (filter #(= (last %) "end") all-traversed)))

(comment
  (paths test-input)
  (let [paths   (paths input)
        paths   (pb-dict paths)]
    (time
      (count-ends (traverse-all paths))))

  (some #(= "c" %) ["a" "b"])
  (end "start-A-b")
)
