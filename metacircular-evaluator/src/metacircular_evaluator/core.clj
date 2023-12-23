(ns metacircular-evaluator.core
  (:gen-class)
  (:require [metacircular-evaluator.expression :refer
             [self-evaluating? variable?]]
            [metacircular-evaluator.state :refer
             [create-state get-env get-expr lookup]]))

(defn eval-state
  [state]
  (let [exp (get-expr state)
        env (get-env state)]
    (cond (self-evaluating? exp) (create-state exp env)
          (variable? exp) (create-state (lookup exp env) env))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
