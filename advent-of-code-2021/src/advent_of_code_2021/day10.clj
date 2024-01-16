(ns advent-of-code-2021.day10
  (:require [advent-of-code-2021.core :refer
             [parse-int parse-lines read-resource]]))

(def input (read-resource "day10.txt"))

(def example-input (read-resource "day10example.txt"))

(defn opening? [ch] (contains? #{\{ \< \[ \(} ch))

(defn closing? [ch] (contains? #{\} \> \] \)} ch))

(def initial-state
  {:stack '(),
   :correct? true,
   :complete? true,
   :first-incorrect nil,
   :to-complete '()})

(defn complete? [state] (:complete state))

(defn correct? [state] (:correct? state))

(defn first-incorrect [state] (:first-incorrect state))

(defn to-complete [state] (:to-complete state))

(defn dispatch-fn
  [_ ch]
  (cond (opening? ch) :opening
        (closing? ch) :closing
        :else :default))

(defmulti update-state dispatch-fn)

(defmethod update-state :opening
  [{:keys [stack to-complete], :as state} ch]
  (let [open-closed {\[ \], \{ \}, \< \>, \( \)}]
    (-> state
        (assoc :stack (cons ch stack)
               :to-complete (cons (open-closed ch) to-complete)
               :complete? false))))

(defmethod update-state :closing
  [{:keys [stack to-complete], :as state} ch]
  (cond
    (empty? stack) (-> state
                       (assoc :stack '()
                              :to-complete '()
                              :complete? false
                              :correct? false
                              :first-incorrect ch))
    (= (first to-complete) ch) (-> state
                                   (assoc :stack (rest stack)
                                          :to-complete (rest to-complete)
                                          :correct? true)
                                   (update :complete?
                                           (fn [_] (empty? (rest stack)))))
    :else (-> state
              (assoc :stack stack
                     :to-complete to-complete
                     :complete? false
                     :correct? false
                     :first-incorrect ch))))

(defmethod update-state :default [_ _] nil)

(defn evaluate-line
  [line]
  (reduce (fn [state ch]
            (if (correct? state) (update-state state ch) (reduced state)))
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


(assert (= (-> "[({(<(())[]>[[{[]{<()<>>"
               (evaluate-line)
               (to-complete))
           '(\} \} \] \] \) \} \) \])))

(assert (= (-> "[(()[<>])]({[<{<<[]>>("
               (evaluate-line)
               (to-complete))
           '(\) \} \> \] \} \))))

(defn autocomplete-score
  [line]
  (let [values {\) 1, \] 2, \} 3, \> 4}]
    (reduce (fn [partial-score value] (+ (* 5 partial-score) (values value)))
      0
      line)))

(defn middle-value
  [numbers]
  (let [sorted (sort numbers)
        n (count sorted)
        mid-index (quot n 2)]
    (if (odd? n)
      (nth sorted mid-index)
      (/ (+ (nth sorted (dec mid-index)) (nth sorted mid-index)) 2))))

(defn get-second-solution
  [input]
  (->> input
       (map evaluate-line)
       (filter correct?)
       (filter (complement complete?))
       (map to-complete)
       (map autocomplete-score)
       (middle-value)))

(assert (= 288957 (get-second-solution (parse-input example-input))))

(assert (= 2389738699 (get-second-solution (parse-input input))))
