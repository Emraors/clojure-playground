(ns metacircular-evaluator.expression)

(def bool #{'TRUE 'FALSE})

(defn self-evaluating? [exp] (or (number? exp) (string? exp) (bool exp)))

(defn variable? [exp] (symbol? exp))

(defn tagged-list? [tag exp] (and (list? exp) (= (first exp) tag)))

(defn definition? [exp] (tagged-list? 'def exp))

(defn lambda? [exp] (tagged-list? 'fn exp))

(defn function? [exp] (tagged-list? 'defn exp))

(defn if? [exp] (tagged-list? 'if exp))

(defn cond? [exp] (tagged-list? 'cond exp))

(defn let? [exp] (tagged-list? 'let exp))
