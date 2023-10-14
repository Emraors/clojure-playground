(ns advent-of-code-2021.day07
    (:require [advent-of-code-2021.core
             :refer [parse-int parse-lines read-resource parse-binary]]
            [clojure.string :as str]))


(def input (read-resource "day07.txt"))

(def example-input (read-resource "day07example.txt"))

(defn parse-input [input]
  (map (comp parse-int
             #(str/trim %))
       (str/split input #"\s*,\s*")))

(defn constant-cost [final-pos initial-pos]
  (abs (- final-pos initial-pos)))

(defn linear-cost [final-pos initial-pos]
  (let [steps (constant-cost final-pos initial-pos)]
    (/ (* steps (inc steps)) 2)))

(defn state-cost [eval-fun positions aiming-position]
  (->> positions
       (map (partial eval-fun aiming-position))
       (apply +)))

(defn min-cost [eval-fun positions]
  (let [[min-pos max-pos] (apply (juxt min max) positions)]
    (->> (range min-pos (inc max-pos))
         (map (partial state-cost eval-fun positions))
         (apply min))))

(defn eval-solution [eval-function input]
  (min-cost eval-function (parse-input input)))


(assert (= 37
           (eval-solution constant-cost example-input)))

(assert (= 356922
           (eval-solution constant-cost input)))

(assert (= 206
           (eval-solution linear-cost example-input)))

(assert (= 100347031
           (eval-solution linear-cost input)))
