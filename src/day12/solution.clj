(ns day12.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [taoensso.tufte :as tufte]))

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

(defn- queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

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
  (let [pbs       (get paths (last traversed))
        visited   (set traversed)
        uppercase (set (filter all-uppercase? pbs))
        pbs       (set/difference pbs visited)
        pbs       (set/union pbs uppercase)]
    pbs))

(defn- traverse [paths traversed]
  (map #(conj traversed %) (possibilities paths traversed)))

(defn- traverse-step [paths all-traversed queue]
  (let [new-traversed (traverse paths (peek queue))
        new-traversed (set new-traversed)
        all-traversed (set/union all-traversed new-traversed)
        new-traversed (filter #(not (= "end" (last %))) new-traversed)
        queue         (pop queue)
        queue         (reduce conj queue new-traversed)]
    [all-traversed queue]))

(defn- traverse-all [paths]
  (loop [all-traversed (set [["start"]])
         queue         (queue [["start"]])]
    (if (not-empty queue)
      (let [[all-traversed queue] (traverse-step paths all-traversed queue)]
        (recur all-traversed queue))
      all-traversed)))

(defn- count-ends [all-traversed]
  (count (filter #(= (last %) "end") all-traversed)))

(defn solution [input]
  (let [paths   (paths input)
        paths   (pb-dict paths)]
    (count-ends (traverse-all paths))))

(tufte/add-basic-println-handler! {})

(comment
  (paths test-input)
  (let [paths   (paths test-input)
        paths   (pb-dict paths)]
    (traverse paths ["start"]))
    ;(time
    ;  (count-ends (traverse-all paths))))

  (tufte/profile ; Profile any `p` forms called during body execution
    {} ; Profiling options; we'll use the defaults for now
    (tufte/p :solution (solution input)))
)
