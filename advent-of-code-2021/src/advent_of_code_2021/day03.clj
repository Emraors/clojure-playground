(ns advent-of-code-2021.day03
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource parse-binary]]
            [clojure.string :as str]))

(def input (read-resource "day03.txt"))

(def example-input
  "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010")

(defn transpose [matrix] (apply mapv vector matrix))

(defn parse-input [input] (parse-lines input))

(defn most-common-bit
  [m]
  (let [f (frequencies m)
        zero-freq (f \0)
        one-freq (f \1)]
    (cond (nil? zero-freq) \1
          (nil? one-freq) \0
          (<= zero-freq one-freq) \1
          :else \0)))

(defn least-common-bit
  [m]
  (let [f (frequencies m)
        zero-freq (f \0)
        one-freq (f \1)]
    (cond (nil? zero-freq) \1
          (nil? one-freq) \0
          (<= zero-freq one-freq) \0
          :else \1)))


(defn gamma-rate
  [report]
  (parse-binary (apply str (map most-common-bit report))))

(defn epsilon-rate
  [report]
  (parse-binary (apply str (map least-common-bit report))))

(defn power-consuption [report] (#(* (gamma-rate %) (epsilon-rate %)) report))

(assert (= 198 (power-consuption (transpose (parse-input example-input)))))

(assert (= 749376 (power-consuption (transpose (parse-input input)))))

;;; Second part;;

(defn eval-rate
  [bits-criteria report]
  (let [current-bits (mapv #(nth % 0) report)
        value (bits-criteria current-bits)
        initial-state {:index 0,
                       :report (filter #(= (nth % 0) value) report)}
        stop-condition (fn [{:keys [report]}] (< 1 (count report)))]
    (->> initial-state
         (iterate (fn [{:keys [index report]}]
                    (let [current-bits (mapv #(nth % index) report)
                          value (bits-criteria current-bits)
                          next-report (filter #(= (nth % index) value) report)]
                      {:index (inc index), :report next-report})))
         (drop-while stop-condition)
         (first)
         (:report)
         (first)
         (parse-binary))))

(defn oxigen-rate [input] (eval-rate most-common-bit input))

(defn co2-scrubbing-rate [input] (eval-rate least-common-bit input))

(defn get-second-solution
  [input]
  (#(* (oxigen-rate %) (co2-scrubbing-rate %)) input))

(assert (= 230 (get-second-solution (parse-input example-input))))

(assert (= 2372923 (get-second-solution (parse-input input))))
