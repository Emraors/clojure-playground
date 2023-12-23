(ns metacircular-evaluator.expression-test
  (:require [clojure.test :refer [deftest is are run-tests testing]]
            [metacircular-evaluator.expression :refer
             [self-evaluating? variable? tagged-list? definition? function?
              lambda? if? cond? let?]]))

(deftest self-evaluating?-test
  (testing "self-evaluating"
           (are [expected actual]
                (= expected (self-evaluating? actual))
                true 1
                true 1.0
                true "a"
                true true
                true false
                false :a
                false '(defn fun [x] x)
                false '(+ 1 2))))

(deftest variable?-test
  (testing "variable"
           (are [expected actual]
                (= expected (variable? actual))
                true 'x
                true 'y
                false 1
                false :a)))

(deftest tagged-list?-test
  (testing "tagged-list"
           (is (= true (tagged-list? '+ '(+ 1 2))))
           (is (= false (tagged-list? '+ '(1 2))))))

(deftest definition?-test
  (testing "definition"
           (is (= true (definition? '(def x 1))))
           (is (= false (definition? '(defn fun [x] x))))))

(deftest function-definition?-test
  (testing "function-definition"
           (is (= true (function? '(defn fun [x] x))))
           (is (= false (function? '(def x 1))))))

(deftest lambda?-test
  (testing "anonymous-function"
           (is (= true (lambda? '(fn [x] x))))
           (is (= false (lambda? '(defn fun [x] x))))))

(deftest if?-test
  (testing "if"
           (is (= true (if? '(if true 1 2))))
           (is (= false (if? '(defn fun [x] x))))))

(deftest cond?-test
  (testing "cond"
           (is (= true
                  (cond? '(cond (true? 1) 1
                                (false? 2) 2))))
           (is (= false (cond? '(defn fun [x] x))))))

(deftest let?-test
  (testing "let"
           (is (= true (let? '(let [x 1] x))))
           (is (= false (let? '(defn fun [x] x))))))

(run-tests)
