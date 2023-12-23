(ns metacircular-evaluator.core-test
  (:require [clojure.test :refer :all]
            [metacircular-evaluator.core :as core]))


(deftest self-evaluating?-test
  (testing "self-evaluating"
    (are [expected actual] (= expected (core/self-evaluating? actual))
      true  1
      true  1.0
      true  "a"
      nil   true
      nil   false
      nil   :a
      nil   '(defn fun [x] x)
      nil   '(+ 1 2))))

(deftest variable?-test
  (testing "variable"
    (are [expected actual] (= expected (core/variable? actual))
      true 'x
      true 'y
      false  1
      false :a)))


(deftest tagged-list?-test
  (testing "tagged-list"
    (is (= true (core/tagged-list? '+ '(+ 1 2))))
    (is (= false (core/tagged-list? '+ '(1 2))))))

(deftest definition?-test
  (testing "definition"
    (is (= true (core/definition? '(def x 1))))
    (is (= false (core/definition? '(defn fun [x] x))))))

(deftest function-definition?-test
  (testing "function-definition"
    (is (= true (core/function? '(defn fun [x] x))))
    (is (= false (core/function? '(def x 1))))))

(deftest lambda?-test
  (testing "anonymous-function"
    (is (= true (core/lambda? '(fn [x] x))))
    (is (= false (core/lambda? '(defn fun [x] x))))))


(deftest if?-test
  (testing "if"
    (is (= true (core/if? '(if true 1 2))))
    (is (= false (core/if? '(defn fun [x] x))))))

(deftest cond?-test
  (testing "cond"
    (is (= true (core/cond? '(cond (true? 1) 1 (false? 2) 2))))
    (is (= false (core/cond? '(defn fun [x] x))))))



(run-tests)
