(ns assembler.core-test
  (:require [clojure.test :refer :all]
            [assembler.core :refer :all]))

(deftest test-extract-command
  (testing "extract command from line"
    (is (= (extract-command "  @R0  ") "@R0"))
    (is (= (extract-command "  D=M   // D = first number") "D=M"))))

(deftest test-a-command-parse
  (testing "parse of @2"
    (is (= (command->code "@2") "0000000000000010"))))

(deftest test-c-command-parse
  (testing "parse of D=A"
    (is (= (command->code "D=A" ) "1110110000010000"))))
