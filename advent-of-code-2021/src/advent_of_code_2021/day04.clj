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

(defn extract-number [state]
  (first (get state :numbers)))

(defn last-extracted-number [state]
  (first (get state :extracted-numbers)))

(defn playable-tables [state]
  (get-in state [:tables :non-winning-tables]))

(defn winning-tables [state]
  (get-in state [:tables :winning-tables]))

(defn parse-input [input]
  (let [numbers (first (str/split input #"\n\n"))
        tables (rest (str/split input #"\n\n"))]
    {:numbers (parse-numbers numbers)
     :extracted-numbers '()
     :tables {:winning-tables '()
              :non-winning-tables (map parse-table tables)}}))

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

(defn mark-and-check-tables
  [extracted-number {:keys [non-winning-tables winning-tables]}]
  (reduce (fn [{:keys [non-winning-tables
                       winning-tables] :as table-state}
               table]
            (let [marked-table (mark-table extracted-number table)]
              (if (bingo? marked-table)
                {:non-winning-tables non-winning-tables
                 :winning-tables (cons marked-table winning-tables)}
                {:non-winning-tables (cons marked-table non-winning-tables)
                 :winning-tables winning-tables})))
          {:non-winning-tables '()
           :winning-tables winning-tables}
          non-winning-tables))

(defn next-state
  [{:keys [:tables :numbers :extracted-numbers] :as state}]
  (let [current-number (extract-number state)]
    (-> state
        (update :numbers rest)
        (update :extracted-numbers conj current-number)
        (update :tables
                (partial
                 mark-and-check-tables
                 current-number)))))

(defn first-winning? [state]
  (not (empty? (winning-tables state))))

(defn last-winning? [state]
  (empty? (playable-tables state)))

(defn second-game [input]
  (->> input
       (parse-input)
       (iterate next-state)
       (drop-while (complement last-winning?))
       (first)))

(defn play-game [winning-condition input]
  (->> input
       (parse-input)
       (iterate next-state)
       (drop-while (complement winning-condition))
       (first)))

(defn get-solution [input winning-condition]
  (let [winning-state (play-game winning-condition input)
        table (first (winning-tables winning-state))
        number (last-extracted-number winning-state)]
    (* number (reduce + (filter #(not= % "X") (flatten table))))))


(assert (= 4512
           (get-solution example-input first-winning?)))
(assert (= 60368
           (get-solution input first-winning?)))

(assert (= 1924
           (get-solution example-input last-winning?)))

(assert (= 17435
           (get-solution input last-winning?)))
