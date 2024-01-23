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

(def test-board
  {[4 3] 1,
   [2 2] 1,
   [0 0] 1,
   [1 0] 1,
   [2 3] 9,
   [3 3] 9,
   [1 1] 9,
   [3 4] 1,
   [4 2] 1,
   [3 0] 1,
   [4 1] 1,
   [1 4] 1,
   [1 3] 9,
   [0 3] 1,
   [2 4] 1,
   [0 2] 1,
   [2 0] 1,
   [0 4] 1,
   [3 1] 9,
   [2 1] 9,
   [4 4] 1,
   [1 2] 9,
   [3 2] 9,
   [0 1] 1,
   [4 0] 1})

(def step-1-board
  {[4 3] 4,
   [2 2] 0,
   [0 0] 3,
   [1 0] 4,
   [2 3] 0,
   [3 3] 0,
   [1 1] 0,
   [3 4] 4,
   [4 2] 5,
   [3 0] 4,
   [4 1] 4,
   [1 4] 4,
   [1 3] 0,
   [0 3] 4,
   [2 4] 5,
   [0 2] 5,
   [2 0] 5,
   [0 4] 3,
   [3 1] 0,
   [2 1] 0,
   [4 4] 3,
   [1 2] 0,
   [3 2] 0,
   [0 1] 4,
   [4 0] 3})

(def step-2-board
  {[4 3] 5,
   [2 2] 1,
   [0 0] 4,
   [1 0] 5,
   [2 3] 1,
   [3 3] 1,
   [1 1] 1,
   [3 4] 5,
   [4 2] 6,
   [3 0] 5,
   [4 1] 5,
   [1 4] 5,
   [1 3] 1,
   [0 3] 5,
   [2 4] 6,
   [0 2] 6,
   [2 0] 6,
   [0 4] 4,
   [3 1] 1,
   [2 1] 1,
   [4 4] 4,
   [1 2] 1,
   [3 2] 1,
   [0 1] 5,
   [4 0] 4})

(assert (= step-1-board
           (-> test-board
               initial-state
               step
               (get :current-grid))))

(assert (= step-2-board
           (-> test-board
               (initial-state)
               step
               step
               (get :current-grid))))

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
