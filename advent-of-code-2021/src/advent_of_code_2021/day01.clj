(ns advent-of-code-2021.day01
  (:require
   [advent-of-code-2021.core :refer [parse-int lines]]))



(def answer (->> "day01.txt"
                 (lines)
                 (map parse-int)
                 (partition 2 1)
                 (map #(apply < %))
                 (filter true?)
                 (count)))


(def answer-2 (->> "day01.txt"
                   (lines)
                   (map parse-int)
                   (partition 3 1)
                   (map #(reduce + %))
                   (partition 2 1)
                   (map #(apply < %))
                   (filter true?)
                   (count)))
