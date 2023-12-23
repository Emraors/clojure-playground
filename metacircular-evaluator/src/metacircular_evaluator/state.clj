(ns metacircular-evaluator.state)

(defrecord State [exp env])

(defn create-state [exp env] (State. exp env))

(defn get-env [state] (:env state))

(defn get-expr [state] (:exp state))

(def initial-state (State. 'NIL {}))

(defn lookup [var env]
  (get env var))
