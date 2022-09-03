(ns day18.solution
  (:require [cheshire.core :as cheshire]
            [shared.util :as util]))

(defn- explode [number]
  number)

(defn- node-or-id [node]
  (if (number? (:n node))
    (:n node)
    (:id node)))

(defn- node [n]
  (assoc {} :n n :id (util/uniqid)))

(defn- graph-add [graph node left right]
  (assoc
    graph
    (:id node)
    {:id (:id node)
     :parent (:parent node)
     :left  {:n (node-or-id left)  :parent (:id node)}
     :right {:n (node-or-id right) :parent (:id node)}}))

(defn- graph [number]
  (loop [stack  [{:n number :id :root :parent :none}]
         graph  {}]
    (if (empty? stack)
      graph
      (let [node         (peek stack)
            stack        (pop stack)
            [left right] (:n node)
            left         {:n left :id (util/uniqid) :parent (:id node)}
            right        {:n right :id (util/uniqid) :parent (:id node)}
            stack        (if (coll? (:n right)) (conj stack right) stack)
            stack        (if (coll? (:n left)) (conj stack left) stack)
            graph        (graph-add graph node left right)]
        (recur stack graph)))))

(defn- right-of-or-parent [graph node-id]
  (let [node   (get graph node-id)
        parent (get graph (:parent node))]
    (if (= (:n (:right parent)) node-id)
      [:parent   (:parent node)]
      [:right-of (:right parent)])))

(defn- right-of-parent [graph node-id]
  (loop [node-id node-id]
    (let [[which right-of-or-parent] (right-of-or-parent graph node-id)]
      (if (= which :parent)
        (recur right-of-or-parent)
        right-of-or-parent))))

(defn- left-of [graph node-id]
  (let [node    (get graph node-id)]
    (println node)
    (loop [node    (get graph (:parent node))
           visited (set [node-id])
           left    (:left node)]
      (println (:n left))
      (println (keyword? (:n left)))
      (if (contains? visited (:parent left))
        (recur
          (get graph (:parent node))
          visited
          (conj left (:id node)))
        left))))

(util/reset-uniqid!)

(comment
  (graph (cheshire/parse-string "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"))

  (let [n [[[9 8] 1] 2]]
    (nth n 0))

  (node [[1 2] 3])

  (explode [1 2])

  (util/reset-uniqid!)
  (right-of-or-parent (graph [[6,[5,[4,[3,2]]]],1]) :8)
  (right-of-or-parent (graph [[6,[5,[4,[3,2]]]],1]) :1)
  (right-of-parent (graph [[6,[5,[4,[3,2]]]],1]) :8)
  (graph [[3 2] 1])
  (graph [[6,[5,[4,[3,2]]]],1])

  (util/uniqid)
)
