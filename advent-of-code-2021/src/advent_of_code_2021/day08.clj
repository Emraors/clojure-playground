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

(defn get-encoding
  [pattern]
  (let [one (set (first (get (group-by count pattern) 2)))
        seven (set (first (get (group-by count pattern) 3)))
        six (first (filter #(not (clojure.set/subset? one %))
                           (map #(set %) (get (group-by count pattern) 6))))
        four (set (first (get (group-by count pattern) 4)))
        eight (set (first (get (group-by count pattern) 7)))
        three (first (filter #(clojure.set/subset? one %)
                             (map #(set %) (get (group-by count pattern) 5))))
        nine (clojure.set/union three four)
        e (clojure.set/difference eight nine)
        b (clojure.set/difference nine three)
        d (clojure.set/difference (clojure.set/difference four one) b)
        g (clojure.set/difference
           (clojure.set/difference (clojure.set/difference nine seven) b)
           d)
        c (clojure.set/difference eight six)
        a (clojure.set/difference seven one)
        f (clojure.set/difference one c)]
    {(first a) \a,
     (first b) \b,
     (first c) \c,
     (first d) \d,
     (first e) \e,
     (first f) \f,
     (first g) \g}))

(defn get-second-solution
  [input]
  (let [mapping {#{\a \b \c \e \f \g} "0",
                 #{\c \f} "1",
                 #{\a \c \d \e \g} "2",
                 #{\a \c \d \f \g} "3",
                 #{\b \c \d \f} "4",
                 #{\a \b \d \f \g} "5",
                 #{\a \b \d \e \f \g} "6",
                 #{\a \c \f} "7",
                 #{\a \b \c \d \e \f \g} "8",
                 #{\a \b \c \d \f \g} "9"}
        decode (fn [encoding values]
                 (apply str
                        (mapv #(get mapping (set (map encoding %))) values)))]
    (apply +
           (map (comp parse-int
                      (fn [val]
                        (let [encoding (get-encoding (:patterns val))]
                          (decode encoding (:outputs val)))))
                input))))

(assert (= 61229 (get-second-solution (parse-input example-input))))

(assert (= 973292 (get-second-solution (parse-input input))))
