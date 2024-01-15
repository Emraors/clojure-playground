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



(def ex
  {:patterns ["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb"
              "fabcd" "edb"],
   :outputs ["fdgacbe" "cefdb" "cefbgd" "gcbe"]})


(defn get-one [pattern] (set (first (get (group-by count pattern) 2))))

(defn get-seven [pattern] (set (first (get (group-by count pattern) 3))))

(defn get-four [pattern] (set (first (get (group-by count pattern) 4))))

(defn get-eight [pattern] (set (first (get (group-by count pattern) 7))))

(defn get-three
  [pattern]
  (first (filter #(clojure.set/subset? (get-one pattern) %)
           (map #(set %) (get (group-by count pattern) 5)))))

(defn get-nine
  [pattern]
  (clojure.set/union (get-three pattern) (get-four pattern)))

(defn get-g
  [pattern]
  (clojure.set/difference (get-eight pattern) (get-nine pattern)))

(defn get-e
  [pattern]
  (clojure.set/difference (get-nine pattern) (get-three pattern)))

(defn get-f
  [pattern]
  (clojure.set/difference (clojure.set/difference (get-four pattern)
                                                  (get-one pattern))
                          (get-g pattern)))

(defn get-zero
  [pattern]
  (clojure.set/difference (get-eight pattern) (get-f pattern)))

(defn get-c
  [pattern]
  (clojure.set/difference (clojure.set/difference (clojure.set/difference
                                                    (get-nine pattern)
                                                    (get-seven pattern))
                                                  (get-e pattern))
                          (get-f pattern)))

(defn get-six
  [pattern]
  (first (filter #(not (clojure.set/subset? (get-one pattern) %))
           (map #(set %) (get (group-by count pattern) 6)))))

(defn get-a
  [pattern]
  (clojure.set/difference (get-eight pattern) (get-six pattern)))

(defn get-five
  [pattern]
  (clojure.set/difference (get-nine pattern) (get-a pattern)))

(defn get-b
  [pattern]
  (clojure.set/difference (get-one pattern) (get-a pattern)))

(defn get-d
  [pattern]
  (clojure.set/difference (get-seven pattern) (get-eight pattern)))



(def mapping {:one ""})


(defn decode [mapping pattern])
