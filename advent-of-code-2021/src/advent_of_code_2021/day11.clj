(ns advent-of-code-2021.day11
    (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource]]))

(def input (read-resource "day11.txt"))

(def example-input (read-resource "day11example.txt"))

(defn parse-input
  [input]
  (map #(map parse-int (re-seq #"." %)) (parse-lines input)))

(defn nth-mth [board x y] (nth (nth board y) x))
