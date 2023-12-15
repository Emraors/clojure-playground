(ns advent-of-code-2021.day02
  (:require [advent-of-code-2021.core :refer [read-resource]]
            [clojure.string :as str]))

(defn format-direction
  [line]
  (let [[direction amount] (str/split line #" ")]
    {(keyword direction) (parse-int amount)}))

(defn parse-moves
  [inputs]
  (->> inputs
       (str/split-lines)
       (map format-direction)))

(defn compute-result [{:keys [horizontal depth]}] (* horizontal depth))

(defn get-answer
  [input update-fn initial-state]
  (->> input
       (read-resource)
       (parse-moves)
       (reduce update-fn initial-state)
       (compute-result)))

(def answer-1
  (get-answer "day02.txt"
              (fn [state next-move]
                (let [direction (first (keys next-move))
                      amount (get next-move direction)]
                  (case direction
                    :forward (update state :horizontal + amount)
                    :down (update state :depth + amount)
                    :up (update state :depth - amount))))
              {:horizontal 0, :depth 0}))

(def answer-2
  (get-answer "day02.txt"
              (fn [state next-move]
                (let [direction (first (keys next-move))
                      amount (get next-move direction)]
                  (case direction
                    :forward (-> state
                                 (update :horizontal + amount)
                                 (update :depth + (* (:aim state) amount)))
                    :down (update state :aim + amount)
                    :up (update state :aim - amount))))
              {:horizontal 0, :depth 0, :aim 0}))
