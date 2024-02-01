(ns advent-of-code-2021.day12
  (:require [advent-of-code-2021.core :refer [parse-lines read-resource]]
            [clojure.string :as str]))

(def example-input (read-resource "day12example.txt"))
(def input (read-resource "day12.txt"))

;; Adapted from:
;; https://subscription.packtpub.com/book/data/9781783284139/1/ch01lvl1sec11/implementing-the-graphs
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

(defn parse-input
  [input]
  (->> input
       parse-lines
       (map #(str/split % #"-"))
       (reduce #(add-node %1 %2 true) {})))

(defn has-twice-visited-small-cave?
  [seen]
  (some (partial <= 2) (vals (filter #(small-cave? (key %)) seen))))

(defn visitable?
  [visited repeat-small? cave]
  (and (not (start-vertex? cave))
       (or (large-cave? cave)
           (end-vertex? cave)
           (and repeat-small? (not (has-twice-visited-small-cave? visited)))
           (not (contains? visited cave)))))

(defn find-paths
  ([cave-maps repeat-small?] (find-paths cave-maps repeat-small? ["start"] {}))
  ([cave-maps repeat-small? path visited]
   (let [last-visited-cave (last path)]
     (if (end-vertex? last-visited-cave)
       [path]
       (->> (cave-maps last-visited-cave)
            (filter (partial visitable? visited repeat-small?))
            (mapcat (fn [cave]
                      (find-paths cave-maps
                                  repeat-small?
                                  (conj path cave)
                                  (update visited cave #(inc (or % 0)))))))))))

(assert (= 10
           (-> example-input
               parse-input
               (find-paths false)
               count)))

(assert (= 3298
           (-> input
               parse-input
               (find-paths false)
               count)))

(assert (= 93572
           (-> input
               parse-input
               (find-paths true)
               count)))
