(ns advent-of-code-2021.day06
  (:require [advent-of-code-2021.core
             :refer [parse-int parse-lines read-resource parse-binary]]
            [clojure.string :as str]))

(def new-timer 8)
(def reset-timer 6)
(def delivery-timer 0)
;; my original solution was too slow, I took the idea from: https://github.com/abyala/advent-2021-clojure/blob/main/docs/day06.md
(def empty-state {0 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0, 8 0})

(def input (read-resource "day06.txt"))

(def example-input (read-resource "day06example.txt"))

(defn parse-input [input]
  (merge
   empty-state
   (frequencies
          (map (comp
                parse-int
                #(str/trim %))
               (str/split input #"\s*,\s*")))))

(defn update-timer [timer]
  (if (zero? timer) reset-timer (dec timer)))

(defn next-generation [state]
  (let [deliveries (state delivery-timer)]
    (-> (reduce-kv (fn [m k v]
                     (update m (update-timer k) + v)) empty-state state)
        (assoc new-timer deliveries))))

(defn nth-generation [n state]
  (-> (iterate next-generation state)
      (nth n)))

(defn eval-solution [input num-days]
  (->>  input
        (parse-input)
        (nth-generation num-days)
        vals
        (apply +)))
