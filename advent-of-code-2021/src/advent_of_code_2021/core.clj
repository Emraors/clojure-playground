(ns advent-of-code-2021.core
  (:require [clojure.java.io :refer [resource]])
  (:require [clojure.string :refer [split-lines]])
  (:gen-class))

(defn parse-int
  [number-string]
  "parses string to integer"
  (Integer/parseInt number-string 10))

(defn parse-long
  [number-string]
  "parses string to long"
  (Long/parseLong number-string))

(defn lines
  [name]
  "reads lines from a resource"
  (->> name
       (resource)
       (slurp)
       (split-lines)))
