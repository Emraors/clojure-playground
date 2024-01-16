(ns advent-of-code-2021.day10
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource]]))

(def input (read-resource "day10.txt"))

(def example-input (read-resource "day10example.txt"))

(defn opening? [ch] (contains? #{\{ \< \[ \(} ch))

(defn closing? [ch] (contains? #{\} \> \] \)} ch))

(def initial-state
  {:stack '(), :correct? true, :complete? true, :first-incorrect nil})

(defn complete? [state] (:complete state))

(defn corrupted? [state] (:correct? state))

(defn first-incorrect [state] (:first-incorrect state))

(defn dispatch-fn
  [_ ch]
  (cond (opening? ch) :opening
        (closing? ch) :closing
        :else :default))

(defmulti update-state dispatch-fn)

(defmethod update-state :opening
  [{:keys [stack], :as state} ch]
  (-> state
      (assoc :stack (cons ch stack))
      (assoc :complete? false)))

(defmethod update-state :closing
  [{:keys [stack], :as state} ch]
  (let [open-closed {\[ \], \{ \}, \< \>, \( \)}]
    (cond
      (empty? stack) (-> state
                         (assoc :stack '())
                         (assoc :complete? false)
                         (assoc :correct? false)
                         (assoc :first-incorrect ch))
      (= (open-closed (first stack)) ch)
        (-> state
            (assoc :stack (rest stack))
            (assoc :correct? true)
            (update :complete? (fn [_] (empty? (rest stack)))))
      :else (-> state
                (assoc :stack stack)
                (assoc :complete? false)
                (assoc :correct? false)
                (assoc :first-incorrect ch)))))

(defmethod update-state :default [_ _] nil)

(defn evaluate-line
  [line]
  (reduce (fn [state ch]
            (if (corrupted? state) (update-state state ch) (reduced state)))
    initial-state
    line))

(defn parse-input [input] (parse-lines input))

(assert (= (-> "{([(<{}[<>[]}>{[]{[(<()>"
               (evaluate-line)
               (first-incorrect))
           \}))

(assert (= (-> "[[<[([]))<([[{}[[()]]]"
               (evaluate-line)
               (first-incorrect))
           \)))

(assert (= (-> "[{[{({}]{}}([{[{{{}}([]"
               (evaluate-line)
               (first-incorrect))
           \]))

(defn get-first-solution
  [input]
  (let [values {\) 3, \] 57, \} 1197, \> 25137}]
    (->> input
         (map (comp values first-incorrect evaluate-line))
         (filter (complement nil?))
         (apply +))))

(assert (= 26397 (get-first-solution (parse-input example-input))))

(assert (= 318099 (get-first-solution (parse-input input))))
