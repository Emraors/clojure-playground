(ns advent-of-code-2021.day11
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource]]
            [clojure.string :as str]))

;; I am trying to explore new way of representing a matrix in Clojure. Instead
;; the
;; representation I used in the previous days, I am trying to use a map of
;; coordinates
;; to values. I am not sure if this is the best way to do it, but I am
;; trying it out.

(def input (read-resource "day11.txt"))

(def example-input (read-resource "day11example.txt"))

(defn parse-input
  [input]
  (->> (parse-lines input)
       (map-indexed (fn [y line] (map-indexed (fn [x c] [[x y] c]) line)))
       (apply concat)
       (reduce (fn [acc [p c]] (assoc acc p ((comp parse-int str) c))) {})))

(def state {:current-grid {}, :to-be-flashed #{}, :num-flashes 0})

(defn increment-values [m] (reduce (fn [acc [k v]] (assoc acc k (inc v))) {} m))

(defn flash? [m p] (= (get m p) 9))

(defn to-be-flashed
  [m]
  (reduce (fn [acc [p _]] (if (flash? m p) (conj acc p) acc)) #{} m))

(assert (= #{[8 8] [0 6] [1 9] [8 0] [1 4] [1 3] [1 8] [4 5] [3 1] [1 6] [2 6]
             [6 2] [6 0] [1 2] [3 2]}
           (-> input
               parse-input
               increment-values
               to-be-flashed)))

(defn get-values
  [board [x y]]
  (let [xs (for [dx [-1 0 1]
                 dy [-1 0 1]
                 :when (or (not= dx 0) (not= dy 0))]
             [(+ x dx) (+ y dy)])]
    (reduce (fn [acc key]
              (if-let [value (get board key)]
                (assoc acc key value)
                acc))
      {}
      xs)))

;; (defn map-values
;;   [m f]
;;   (let [keys (keys m) values (mapv f (vals m))] (zipmap keys values)))


(defn flash [board [x y]])

