(ns advent-of-code-2021.day05
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource parse-binary]]
            [clojure.string :as str]))

(def input (read-resource "day05.txt"))

(def example-input (read-resource "day05example.txt"))

(defn parse-hydrothermal
  [line]
  (let [[start end] (str/split line #"\s*->\s*")
        [sx sy] (map parse-int (str/split start #","))
        [ex ey] (map parse-int (str/split end #","))]
    {:start {:x sx, :y sy}, :end {:x ex, :y ey}}))

(defn parse-input [input] (map parse-hydrothermal (parse-lines input)))

;; a segment is just a list of points, where a point is a map of the form
;; {:x x_0 :y y_0}

(defn create-segment
  [{:keys [start end]}]
  (let [dx (- (:x end) (:x start))
        dy (- (:y end) (:y start))]
    (if (not= 0 dx)
      (for [x (if (< 0 dx)
                (range (:x start) (inc (:x end)))
                (range (:x end) (inc (:x start))))]
        {:x x,
         :y ((fn [x] (+ (:y start) (* (/ dy dx) (- x (:x start))))) x)})
      (for [y (if (< 0 dy)
                (range (:y start) (inc (:y end)))
                (range (:y end) (inc (:y start))))]
        {:x (:x start), :y y}))))

(defn or-pred [& preds] (fn [x] (some #(% x) preds)))

(defn horizontal? [{:keys [start end]}] (= (:x start) (:x end)))

(defn vertical? [{:keys [start end]}] (= (:y start) (:y end)))

(defn diagonal?
  [{:keys [start end]}]
  (let [dx (- (:x end) (:x start))
        dy (- (:y end) (:y start))]
    (= (abs dx) (abs dy))))

(defn eval-solution
  [input & preds]
  (->> input
       (parse-input)
       (filter (apply or-pred preds))
       (mapcat create-segment)
       (frequencies)
       (filter (fn [[_ v]] (> v 1)))
       (count)))

(assert (= 5 (eval-solution example-input vertical? horizontal?)))

(assert (= 6548 (eval-solution input vertical? horizontal?)))

(assert (= 12 (eval-solution example-input vertical? horizontal? diagonal?)))

(assert (= 19663 (eval-solution input vertical? horizontal? diagonal?)))
