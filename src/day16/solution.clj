(ns day16.solution
  (:require [clojure.string :as str]
            [day16.input]
            [shared.util :as util]))


(def hex-to-binary-rules
"0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111")

(def hex-to-binary-dict
  (->> (str/split hex-to-binary-rules #"\n")
       (map #(str/split % #" = "))
       (map (fn [[a b]] into [(first a) b]))
       (into {})))

(defn- hex-to-binary [hex]
  (get hex-to-binary-dict hex))

(defn- line-to-binary [line]
  (->> (map hex-to-binary line)
       (str/join)))

(defn- decimal [binary-str]
  (Long/parseLong binary-str 2))

(defn- digest [line, n]
  (map #(apply str %) (split-at n (seq line))))

(defn- packet-create [rest]
  (let [[version rest] (digest rest 3)
        [type-id rest] (digest rest 3)
        version        (decimal version)
        type-id        (decimal type-id)
        packet         {:version version :type-id type-id}]
    [packet rest]))

(defn- literal-value [packet rest]
  (loop [packet packet
         rest   rest]
    (let [[value       rest]  (digest rest 5)
          [last-value? value] (digest value 1)
          packet              (assoc packet :value (str (:value packet) value))]
      (if (= last-value? "1")
        (recur packet rest)
        [(assoc packet :value (decimal (:value packet))) rest]))))

(declare parse-line)

(defn- packet-add [packet sub-packet]
  (assoc packet :sub-packets (conj (:sub-packets packet) sub-packet)))

(defn- length-15-sub-packet [packet rest]
  (let [[sub-packet rest] (parse-line rest)
        packet      (packet-add packet sub-packet)]
    [packet rest]))

(defn- sub-packets-length-15 [packet rest]
  (let [[packets-length rest]  (digest rest 15)
        packets-length         (decimal packets-length)
        [sub-packet-rest
         rest]      (digest rest packets-length)]
    (loop [packet packet
           sub-packet-rest sub-packet-rest]
      (if (= (count sub-packet-rest) 0)
        [packet rest]
        (let [[packet
               sub-packet-rest] (length-15-sub-packet packet sub-packet-rest)]
          (recur packet sub-packet-rest))))))

(defn- sub-packets-by-count [packet rest]
  (let [[packet-count rest] (digest rest 11)
        packet-count        (decimal packet-count)]
    (loop [i 0
           packet packet
           rest rest]
      (if (= i packet-count)
        [packet rest]
        (let [[sub-packet rest] (parse-line rest)
              packet            (packet-add packet sub-packet)]
          (recur (inc i) packet rest))))))

(defn- operator [packet rest]
  (let [[length-id rest] (digest rest 1)
        packet           (assoc packet :length-id length-id)]
    (case length-id
      "0" (sub-packets-length-15 packet rest)
      "1" (sub-packets-by-count  packet rest))))

(defn- parse-by-type [packet rest]
  (case (:type-id packet)
    4 (literal-value packet rest)
    (operator packet rest)))

(defn- parse-line [line]
  (let [[packet rest]        (packet-create line)
        [packet rest]        (parse-by-type packet rest)]
    [packet rest]))

(defn- flatten-packet [packet]
  (loop [packets []
         queue   (util/queue [packet])]
    (if (empty? queue)
      packets
      (let [packet  (peek queue)
            queue   (pop queue)
            queue   (reduce conj queue (:sub-packets packet))
            packets (conj packets (dissoc packet :sub-packets))]
        (recur packets queue)))))

(defn- version-sum [packet]
  (->> (flatten-packet packet)
       (map :version)
       (reduce +)))

(defn- part1 [input]
  (let [binary             (line-to-binary input)
        [packet remainder] (parse-line binary)]
    (version-sum packet)))

(comment
  (next (line-to-binary "D2FE28") 3)
  (parse-line (line-to-binary "D2FE28"))
  (parse-line (line-to-binary "38006F45291200"))
  (parse-line (line-to-binary "EE00D40C823060"))
  (line-to-binary "8A004A801A8002F478")
  (parse-line (line-to-binary "8A004A801A8002F478"))
  (version-sum (first (parse-line (line-to-binary "620080001611562C8802118E34"))))
  (part1 "620080001611562C8802118E34")
  (part1 day16.input/input)
  (parse-line (line-to-binary day16.input/input))
)
