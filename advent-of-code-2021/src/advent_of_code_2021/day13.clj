(ns advent-of-code-2021.day13
  (:require [advent-of-code-2021.core :refer [parse-int read-resource]]
            [clojure.string :as str]))

(def example-input (read-resource "day13example.txt"))
(def input (read-resource "day13.txt"))

(defn parse-instruction
  [line]
  (let [[_ axis amount] (re-matches #"fold along ([xy])=(\d+)" line)]
    [(keyword axis) (parse-int amount)]))

(defn parse-input
  [input]
  (let [[dots folds] (str/split input #"\n\n")]
    {:paper (->> (str/split-lines dots)
                 (map #(mapv parse-int (str/split % #",")))
                 set),
     :instructions (map parse-instruction (str/split-lines folds))}))

(defn get-paper [state] (:paper state))
(defn get-instructions [state] (:instructions state))

;; In this case a cond would have been fine
;; (defn apply-symmetry
;;   [direction fold-line [x y]]
;;   (case direction
;;     :y (if (< y fold-line) [x y] [x (- (* 2 fold-line) y)])
;;     :x (if (< x fold-line) [x y] [(- (* 2 fold-line) x) y])))

(defmulti apply-symmetry (fn [direction _ _] direction))

(defmethod apply-symmetry :x
  [_ fold-line [x y]]
  (if (< x fold-line) [x y] [(- (* 2 fold-line) x) y]))

(defmethod apply-symmetry :y
  [_ fold-line [x y]]
  (if (< y fold-line) [x y] [x (- (* 2 fold-line) y)]))

(defn eval-instruction
  [paper [direction fold-line]]
  (reduce (fn [folded coord]
            (conj folded (apply-symmetry direction fold-line coord)))
    #{}
    paper))

(defn first-part
  [input]
  (-> input
      parse-input
      (#(eval-instruction (get-paper %)
                          (-> %
                              get-instructions
                              first)))
      count))

(assert (= 17 (first-part example-input)))
(assert (= 695 (first-part input)))

;; Taken from here (only this):
;; https://github.com/abyala/advent-2021-clojure/blob/main/docs/day13.md
(defn display
  [dots]
  (let [min-x (apply min (map first dots))
        min-y (apply min (map second dots))
        max-x (apply max (map first dots))
        max-y (apply max (map second dots))]
    (run!
      println
      (map (fn [y]
             (apply str
               (map #(if (dots [% y]) \# \space) (range min-x (inc max-x)))))
        (range min-y (inc max-y))))))

(defn second-part
  [input]
  (->> input
       parse-input
       (#(reduce eval-instruction (get-paper %) (get-instructions %)))
       display))
