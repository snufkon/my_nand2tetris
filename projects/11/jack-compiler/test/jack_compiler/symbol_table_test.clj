(ns jack-compiler.symbol-table-test
  (:require [clojure.test :refer :all]
            [jack-compiler.symbol-table :as stable]))

(defn- setup
  []
  (stable/init)
  (stable/define "nAccounts" "int" "static")
  (stable/define "bankCommission" "int" "static")
  (stable/define "id" "int" "field")
  (stable/define "owner" "String" "field")
  (stable/define "balance" "int" "field")
  (stable/define "this" "BankAccount" "arg")
  (stable/define "sum" "int" "arg")
  (stable/define "from" "BankAccount" "arg")
  (stable/define "when" "Date" "arg")
  (stable/define "i" "int" "var")
  (stable/define "j" "int" "var")
  (stable/define "due" "Date" "var"))


;;;
;;; tests
;;; 

(deftest kind-of-test
  (setup)
  (is (= "STATIC" (stable/kind-of "nAccounts")))
  (is (= "FIELD"  (stable/kind-of "id")))
  (is (= "ARG"    (stable/kind-of "this")))
  (is (= "VAR"    (stable/kind-of "i")))
  (is (= "NONE"   (stable/kind-of "hoge"))))

(deftest type-of-test
  (setup)
  (is (= "int"         (stable/type-of "nAccounts")))
  (is (= "String"      (stable/type-of "owner")))
  (is (= "BankAccount" (stable/type-of "this")))
  (is (= "Date"        (stable/type-of "due"))))

(deftest index-of-test
  (setup)
  (is (= 0 (stable/index-of "nAccounts")))
  (is (= 1 (stable/index-of "bankCommission")))
  (is (= 2 (stable/index-of "balance")))
  (is (= 3 (stable/index-of "when")))
  (is (= 1 (stable/index-of "j"))))

(deftest var-count-test
  (setup)
  (is (= 2 (stable/var-count "STATIC")))
  (is (= 3 (stable/var-count "FIELD")))
  (is (= 4 (stable/var-count "ARG")))
  (is (= 3 (stable/var-count "VAR"))))

(deftest start-subroutine-test
  (setup)
  (is (= 2 (stable/var-count "STATIC")))
  (is (= 3 (stable/var-count "FIELD")))
  (is (= 4 (stable/var-count "ARG")))
  (is (= 3 (stable/var-count "VAR")))
  (stable/start-subroutine)
  (stable/define "k" "int" "var")
  (stable/define "l" "int" "var")
  (is (= 2 (stable/var-count "STATIC")))
  (is (= 3 (stable/var-count "FIELD")))
  (is (= 0 (stable/var-count "ARG")))
  (is (= 2 (stable/var-count "VAR"))))
