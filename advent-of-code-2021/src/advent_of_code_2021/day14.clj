(ns advent-of-code-2021.day14
  (:require [advent-of-code-2021.core :refer [parse-int read-resource]]
            [clojure.string :as str]))

(def example-input (read-resource "day14example.txt"))
(def input (read-resource "day14.txt"))

(defn parse-rule
  [line]
  (let [[_ pair result] (re-matches #"([A-Z]{2}) -> ([A-Z])" line)]
    {pair result}))

(defn parse-input
  [input]
  (let [[pattern rules] (str/split input #"\n\n")]
    {:pattern pattern,
     :rules-map (into {} (map parse-rule (str/split-lines rules)))}))

(defn get-pattern [state] (:pattern state))
(defn get-rules [state] (:rules-map state))


;; My original solution worked for the first part, but it is too slow for the
;; second one.
;; (defn apply-rule
;;   [rules-map [l r]]
;;   (let [result (get rules-map (str l r))]
;;     (if result (str l result r) (str l r))))

;; (defn react
;;   [rule-map reagent]
;;   (->> reagent
;;        (partition 2 1)
;;        (map (partial apply-rule rule-map))
;;        (apply str)))


;; (defn first-solution
;;   [input]
;;   (let [pattern (-> input
;;                     parse-input
;;                     get-pattern)
;;         rules-map (-> input
;;                       parse-input
;;                       get-rules)]
;;     (->> pattern
;;          (iterate (partial react rules-map))
;;          (take 40)
;;          last
;;          count)))
