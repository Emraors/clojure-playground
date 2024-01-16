(ns advent-of-code-2021.day09
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource parse-binary]]
            [clojure.string :as str]))

(def input (read-resource "day09.txt"))

(def example-input (read-resource "day09example.txt"))

(defn parse-input
  [input]
  (map #(map parse-int (re-seq #"." %)) (parse-lines input)))

(defn nth-mth [board x y] (nth (nth board y) x))

(defn nearest-neigh
  [board x y]
  (let [row-lenght (count (first board))
        col-lenght (count board)]
    (for [i (range (max 0 (dec x)) (min row-lenght (+ 2 x)))
          j (range (max 0 (dec y)) (min col-lenght (+ 2 y)))
          :when (not= (list x y) (list i j))]
      (list i j))))

(defn low-point?
  [board x y]
  (let [value (nth-mth board x y)
        nn-cord (nearest-neigh board x y)
        values (map (fn [[x y]] (nth-mth board x y)) nn-cord)]
    (every? #(> % value) values)))

(defn map-low-points
  [map-fn board]
  (let [cols (count board)
        rows (count (first board))]
    (for [x (range rows)
          y (range cols)
          :when (low-point? board x y)]
      (map-fn board x y))))

(defn get-first-solution
  [input]
  (->> input
       (map-low-points (comp inc nth-mth))
       (apply +)))

(assert (= 15 (get-first-solution (parse-input example-input))))

(assert (= 502 (get-first-solution (parse-input input))))


(defn basin-border? [board x y] (= (nth-mth board x y) 9))

(defn inside?
  [board x y]
  (let [row-lenght (count (first board))
        col-lenght (count board)]
    (and (< x row-lenght) (< y col-lenght) (<= 0 x) (<= 0 y))))

(defn fill
  [board x y]
  (loop [stack [[x y]]
         filled #{}]
    (if (empty? stack)
      filled
      (let [[cx cy :as current] (first stack)
            remaining-stack (rest stack)]
        (if (and (inside? board cx cy)
                 (not (basin-border? board cx cy))
                 (not (contains? filled current)))
          (recur (conj remaining-stack
                       [(inc cx) cy]
                       [cx (inc cy)]
                       [(dec cx) cy]
                       [cx (dec cy)])
                 (conj filled current))
          (recur remaining-stack filled))))))

(defn get-second-solution
  [input]
  (->> input
       (map-low-points (comp count fill))
       (#(take 3 (sort > %)))
       (apply *)))

(assert (= 1134 (get-second-solution (parse-input example-input))))

(assert (= 1330560 (get-second-solution (parse-input input))))
