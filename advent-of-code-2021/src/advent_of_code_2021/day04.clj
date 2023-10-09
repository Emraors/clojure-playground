(ns advent-of-code-2021.day04
  (:require [advent-of-code-2021.core
             :refer [parse-int parse-lines read-resource parse-binary]]
            [clojure.string :as str]))

(def marker "X")

(def input (read-resource "day04.txt"))

(def example-input (read-resource "day04example.txt"))

(defn parse-numbers [input]
  (map parse-int (str/split input #",")))

(defn parse-table [input]
  (let [line->ints (comp
                    (map str/trim)
                    (map #(str/split % #"\s+"))
                    (map #(map parse-int %)))]
    (into '() line->ints (parse-lines input))))

;; I might want to change the representation of the state later
(defn get-number [state]
  (first (get state :numbers)))

(defn get-exatracted-number [state]
  (get state :extraxted-number))

(defn get-next-number [state]
  (rest (get state :numbers)))

(defn get-tables [state]
  (get state :tables))

(defn get-winning-tables [state]
  (get state :winning-tables))

(defn create-state [extracted numbers tables winning-tables]
  {:numbers numbers
   :tables tables
   :extraxted-number extracted
   :winning-tables winning-tables})


(defn parse-input [input]
  (let [numbers (first (str/split input #"\n\n"))
        tables (rest (str/split input #"\n\n"))]
   (create-state
    nil
    (parse-numbers numbers)
    (map parse-table tables)
    '())))


(defn winning? [state]
  (not (empty? (get-winning-tables state))))

(defn mark-table [number table]
  (map (fn [row]
         (map (fn [elem]
                (if (= elem number) marker elem))
              row))
       table))

(defn bingo? [table]
  (let [rows (apply map list table)
        winning-condition (fn [el]
                            (boolean
                             (some #(every? #{marker} %)
                                   el)))]
    (or (winning-condition table)
        (winning-condition rows))))

(defn find-winning-tables [tables]
  (reduce (fn [winning-tables table]
            (if (bingo? table)
              (cons table winning-tables)
              winning-tables))
          '()
          tables))

;;fix this
(defn next-state [current-state]
  (let [current-number (get-number current-state)
        next-numbers (get-next-number current-state)
        tables (get-tables current-state)
        next-tables (map (partial mark-table current-number) tables)
        winning-tables (find-winning-tables next-tables)]
    (create-state current-number
                  next-numbers
                  next-tables
                  winning-tables)))


(defn game [input]
    (->> input
         (parse-input)
         (iterate next-state)
         (drop-while (complement winning?))
         (first)))

(defn get-first-solution [input]
  (let [winning-state (game input)
        table (first (get-winning-tables winning-state))
        number (get-exatracted-number winning-state)]
    (* number (reduce + (filter #(not= % "X") (flatten table))))))


(assert (= 4512
           (get-first-solution example-input)))
