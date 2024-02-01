(ns advent-of-code-2021.day12
  (:require [advent-of-code-2021.core :refer
             [parse-lines read-resource]]
            [clojure.string :as str]))

(def example-input (read-resource "day12example.txt"))
(def input (read-resource "day12.txt"))

;; Adapted from: https://subscription.packtpub.com/book/data/9781783284139/1/ch01lvl1sec11/implementing-the-graphs
;; I like the fact that it takes care also of the bidirectional case
(defn add-node
  ([graph [from to]] (add-node graph [from to] false))
  ([graph [from to] bidirectional?]
   ((if bidirectional? #(add-node % [to from] false) identity)
    (update graph from #(if (nil? %) #{to} (conj % to))))))

(defn start-vertex? [vertex] (= vertex "start"))
(defn end-vertex? [vertex] (= vertex "end"))
(defn small-cave? [vertex] (re-find #"[a-z]+" vertex))
(defn large-cave? [vertex] (re-find #"[A-Z]+" vertex))

(defn visited? [vertex visited] (contains? visited vertex))


(defn parse-input [input]
  (->> input
       parse-lines
       (map #(str/split %  #"-"))
       (reduce #(add-node %1 %2 true) {})))
