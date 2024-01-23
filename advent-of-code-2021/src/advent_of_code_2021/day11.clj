(ns advent-of-code-2021.day11
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource]]))

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

(defn flash? [m p] (> (get m p) 9))

(defn flashed? [flashed coord] (contains? flashed coord))

(defn to-be-flashed
  ([grid flashed]
   (reduce (fn [acc [p _]]
             (if (and (flash? grid p) (not (flashed? flashed p)))
               (conj acc p)
               acc))
     #{}
     grid))
  ([grid] (to-be-flashed grid #{})))

(defn initial-state
  [board]
  {:current-grid board, :to-be-flashed #{}, :flashed #{}, :num-flashes 0})

(defn increment-non-flashed
  [current-grid flashed]
  (reduce (fn [acc [k v]]
            (if (flashed? flashed [k v]) acc (assoc acc k (inc v))))
    {}
    current-grid))

(defn flash-local
  [{:keys [current-grid flashed], :as state}]
  (-> state
      (assoc :current-grid (increment-non-flashed current-grid flashed))
      (assoc :to-be-flashed (to-be-flashed (increment-non-flashed current-grid
                                                                  flashed)
                                           flashed))))

(defn flash-all
  [{:keys [current-grid to-be-flashed flashed num-flashes], :as state}]
  (if (empty? to-be-flashed)
    state
    (let [to-flash (first to-be-flashed)
          local-grid (get-values current-grid to-flash)
          local-state (flash-local {:current-grid local-grid, :flashed flashed})
          local-grid (local-state :current-grid)
          to-be-flashed-local (local-state :to-be-flashed)
          new-state {:current-grid (merge current-grid local-grid),
                     :to-be-flashed (clojure.set/union to-be-flashed-local
                                                       (disj to-be-flashed
                                                             to-flash)),
                     :num-flashes (inc num-flashes),
                     :flashed (conj flashed to-flash)}]
      (flash-all new-state))))

(defn reset-flashed
  [{:keys [current-grid flashed], :as state}]
  (-> state
      (assoc :current-grid (reduce (fn [acc coord] (assoc acc coord 0))
                             current-grid
                             flashed))
      (assoc :flashed #{})))


(defn step
  [state]
  (let [increment-grid (increment-non-flashed (state :current-grid)
                                              (state :flashed))
        to-be-flashed (to-be-flashed (increment-non-flashed (state
                                                              :current-grid)
                                                            (state :flashed)))]
    (-> state
        (assoc :current-grid increment-grid)
        (assoc :to-be-flashed to-be-flashed)
        flash-all
        reset-flashed)))

(defn get-first-solution
  [input]
  (->> input
       initial-state
       (iterate step)
       (take 101)
       last
       (:num-flashes)))

(assert (= 1656
           (-> example-input
               parse-input
               get-first-solution)))

(assert (= 1637
           (-> input
               parse-input
               get-first-solution)))

(defn all-flash? [{:keys [current-grid]}] (every? #(= % 0) (vals current-grid)))

(defn get-second-solution
  [input]
  (->> input
       initial-state
       (iterate step)
       (take-while (complement all-flash?))
       count))

(assert (= 195
           (-> example-input
               parse-input
               get-second-solution)))

(assert (= 242
           (-> input
               parse-input
               get-second-solution)))
