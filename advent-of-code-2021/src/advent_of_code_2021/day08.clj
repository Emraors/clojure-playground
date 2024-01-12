(ns advent-of-code-2021.day08
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource parse-binary]]
            [clojure.string :as str]))

(def example-input (read-resource "day08example.txt"))

(def input (read-resource "day08.txt"))

(defrecord Input [patterns outputs])

(defn parse-line
  [input]
  (let [parts (clojure.string/split input #" \| ")
        first-list (clojure.string/split (first parts) #"\s+")
        second-list (clojure.string/split (second parts) #"\s+")]
    (Input. first-list second-list)))

(defn parse-input [input] (map parse-line (parse-lines input)))

(->> '("asf" "asfsss")
     (filter (fn [x]
               (let [count (count x)]
                 (or (= count 3) (= count 7) (= count 4) (= count 2)))))
     (count))

(defn get-first-solution
  [inputs]
  (let [outputs (map :outputs inputs)]
    (->> outputs
         (map (fn [output]
                (->> output
                     (filter (fn [x]
                               (let [cnt (count x)]
                                 (or (= cnt 3) (= cnt 7) (= cnt 4) (= cnt 2)))))
                     count)))
         (apply +))))

(assert (= (get-first-solution (parse-input example-input)) 26))

(assert (= (get-first-solution (parse-input input)) 294))


(def ex {:patterns
 ["be"
  "cfbegad"
  "cbdgef"
  "fgaecd"
  "cgeb"
  "fdcge"
  "agebfd"
  "fecdb"
  "fabcd"
  "edb"],
 :outputs ["fdgacbe" "cefdb" "cefbgd" "gcbe"]})
