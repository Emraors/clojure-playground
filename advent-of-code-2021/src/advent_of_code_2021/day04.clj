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

(defn get-next-numbers [state]
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
  (reduce (fn [{:keys [non-winning-tables winning-tables] :as state}
               table]
            (if (bingo? table)
              {:non-winning-tables (remove
                                    #(= % table)
                                    non-winning-tables)
               :winning-tables (cons table winning-tables)}
              state))
          {:non-winning-tables tables
           :winning-tables '()}
          tables))

(defn get-non-winning-tables [tables]
  ((comp #(get % :non-winning-tables)
         find-winning-tables)
   tables))

(defn extract-winning-tables [tables]
  ((comp #(get % :winning-tables)
         find-winning-tables)
   tables))

#_
(defn mark-and-check-tables [extracted-number
                             {:keys [winning-tables
                                     non-winning-tables]
                              :as tables}]
  (reduce (fn [{:keys [non-winning-tables
                       winning-tables] :as table-state}
               table]
            (let [marked-table (mark-table extracted-number table)]
              (if (bingo? marked-table)
                {:non-winning-tables non-winning-tables
                 :winning-tables (cons table winning-tables)}
                {:non-winning-tables (cons marked-table non-winning-tables)
                 :winning-tables winning-tables})))
            {:non-winning-tables '()
             :winning-tables '()}
            tables))

;;fix this
(defn next-state [current-state]
  (let [current-number (get-number current-state)
        next-numbers (get-next-numbers current-state)
        tables (get-tables current-state)
        marked (map (partial mark-table current-number) tables)
        next-tables (get-non-winning-tables marked)
        winning-tables (extract-winning-tables marked)]
    {:numbers next-numbers
     :tables next-tables
     :winning-tables (concat winning-tables
                             (get-winning-tables current-state))}))

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

#_(assert (= 4512
             (get-first-solution example-input)))
#_(assert (= 60368
             (get-first-solution input)))
