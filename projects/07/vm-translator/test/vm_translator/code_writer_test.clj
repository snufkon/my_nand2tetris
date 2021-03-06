(ns vm-translator.code-writer-test
  (:require [clojure.test :refer :all]
            [clojure.string :as cstr]
            [vm-translator.code-writer :refer :all]))

(def add-codes ["@SP"
               "M=M-1"
               "A=M"
               "D=M"
               "@SP"
               "M=M-1"
               "A=M"
               "M=M+D"
               "@SP"
               "M=M+1"])

(def sub-codes ["@SP"
               "M=M-1"
               "A=M"
               "D=M"
               "@SP"
               "M=M-1"
               "A=M"
               "M=M-D"
               "@SP"
               "M=M+1"])

(def neg-codes ["@SP"
               "M=M-1"
               "A=M"
               "M=-M"
               "@SP"
               "M=M+1"])

(def eq-codes ["@SP"
              "M=M-1"
              "A=M"
              "D=M"
              "@SP"
              "M=M-1"
              "A=M"
              "D=M-D"
              "@EQ"
              "D;JEQ"
              "@SP"
              "A=M"
              "M=0"
              "@SP"
              "M=M+1"
              "@EQ_END"
              "0;JMP"
              "(EQ)"
              "@SP"
              "A=M"
              "M=-1"
              "@SP"
              "M=M+1"
              "(EQ_END)"])

(def gt-codes ["@SP"
              "M=M-1"
              "A=M"
              "D=M"
              "@SP"
              "M=M-1"
              "A=M"
              "D=M-D"
              "@GT"
              "D;JGT"
              "@SP"
              "A=M"
              "M=0"
              "@SP"
              "M=M+1"
              "@GT_END"
              "0;JMP"
              "(GT)"
              "@SP"
              "A=M"
              "M=-1"
              "@SP"
              "M=M+1"
              "(GT_END)"])

(def lt-codes ["@SP"
              "M=M-1"
              "A=M"
              "D=M"
              "@SP"
              "M=M-1"
              "A=M"
              "D=M-D"
              "@LT"
              "D;JLT"
              "@SP"
              "A=M"
              "M=0"
              "@SP"
              "M=M+1"
              "@LT_END"
              "0;JMP"
              "(LT)"
              "@SP"
              "A=M"
              "M=-1"
              "@SP"
              "M=M+1"
              "(LT_END)"])

(def and-codes ["@SP"
              "M=M-1"
              "A=M"
              "D=M"
              "@SP"
              "M=M-1"
              "A=M"
              "M=M&D"
              "@SP"
              "M=M+1"])

(def or-codes ["@SP"
              "M=M-1"
              "A=M"
              "D=M"
              "@SP"
              "M=M-1"
              "A=M"
              "M=M|D"
              "@SP"
              "M=M+1"])

(def not-codes ["@SP"
               "M=M-1"
               "A=M"
               "M=!M"
               "@SP"
               "M=M+1"])

(def push-constant-1-codes
  ["@1"
   "D=A"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(def push-local-0-codes
  ["@0"
   "D=A"
   "@LCL"
   "A=M+D"
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(def push-that-5-codes
  ["@5"
   "D=A"
   "@THAT"
   "A=M+D"
   "D=M"   
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(def push-temp-6-codes
  ["@6"
   "D=A"
   "@R5"
   "A=A+D"
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(def push-static-3-codes
  ["@Sample.3"
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(def pop-local-10-codes
  ["@10"
   "D=A"
   "@LCL"
   "D=M+D"
   "@R13"
   "M=D"
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@R13"
   "A=M"
   "M=D"])

(def pop-argument-1-codes
  ["@1"
   "D=A"
   "@ARG"
   "D=M+D"
   "@R13"
   "M=D"
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@R13"
   "A=M"
   "M=D"])

(def pop-temp-6-codes
  ["@6"
   "D=A"
   "@R5"
   "D=A+D"
   "@R13"
   "M=D"
   "@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@R13"
   "A=M"
   "M=D"])

(def pop-static-1-codes
  ["@SP"
   "M=M-1"
   "A=M"
   "D=M"
   "@Sample.1"
   "M=D"])


(defn- replace-label-code
  [code labels]
  (let [patterns (map #(re-pattern (str % "\\d+")) labels)]
    (loop [lbs labels  pts patterns c code]
      (if (empty? lbs)
        c
        (recur (rest lbs) (rest pts)
               (cstr/replace c (first pts) (first lbs)))))))

(defn- replace-label-codes
  [codes & labels]
  (for [c codes]
    (replace-label-code c labels)))

(deftest test-command->codes
  (testing "arithmetic commands"
    (testing "'add' command -> codes"
      (is (= add-codes (command->codes {:type "C_ARITHMETIC"
                                       :name "add"}))))
    (testing "'sub' command -> codes"
      (is (= sub-codes (command->codes {:type "C_ARITHMETIC"
                                       :name "sub"}))))
    (testing "'neg' command -> codes"
      (is (= neg-codes (command->codes {:type "C_ARITHMETIC"
                                       :name "neg"}))))
    (testing "'eq' command -> codes"
      (is (= eq-codes
             (-> (command->codes {:type "C_ARITHMETIC"
                                  :name "eq"})
                 (replace-label-codes "EQ" "EQ_END")))))
    (testing "'gt' command -> codes"
      (is (= gt-codes
             (-> (command->codes {:type "C_ARITHMETIC"
                                  :name "gt"})
                 (replace-label-codes "GT" "GT_END")))))
    (testing "'lt' command -> codes"
      (is (= lt-codes
             (-> (command->codes {:type "C_ARITHMETIC"
                                  :name "lt"})
                 (replace-label-codes "LT" "LT_END")))))
    (testing "'and' command -> codes"
      (is (= and-codes (command->codes {:type "C_ARITHMETIC"
                                       :name "and"}))))
    (testing "'or' command -> codes"
      (is (= or-codes (command->codes {:type "C_ARITHMETIC"
                                      :name "or"}))))
    (testing "'not' command -> codes"
      (is (= not-codes (command->codes {:type "C_ARITHMETIC"
                                        :name "not"})))))

  (testing "push command"
    (testing "'push constant 1' command -> codes"
      (is (= push-constant-1-codes (command->codes {:type "C_PUSH"
                                                    :name "push"
                                                    :segment "constant"
                                                    :value "1"
                                                    :path "Sample.vm"}))))
    (testing "'push local 0' command -> codes"
      (is (= push-local-0-codes (command->codes {:type "C_PUSH"
                                                 :name "push"
                                                 :segment "local"
                                                 :value "0"
                                                 :path "Sample.vm"}))))
    (testing "'push that 5' command -> codes"
      (is (= push-that-5-codes (command->codes {:type "C_PUSH"
                                                :name "push"
                                                :segment "that"
                                                :value "5"
                                                :path "Sample.vm"}))))
    (testing "'push temp 6' command -> codes"
      (is (= push-temp-6-codes (command->codes {:type "C_PUSH"
                                                :name "push"
                                                :segment "temp"
                                                :value "6"
                                                :path "Sample.vm"}))))
    (testing "'push static 3' command -> codes"
      (is (= push-static-3-codes (command->codes {:type "C_PUSH"
                                                  :name "push"
                                                  :segment "static"
                                                  :value "3"
                                                  :path "Sample.vm"})))))

  (testing "pop command"
    (testing "'pop local 10' command -> codes"
      (is (= pop-local-10-codes (command->codes {:type "C_POP"
                                                 :name "pop"
                                                 :segment "local"
                                                 :value "10"
                                                 :path "Sample.vm"
                                                 }))))
    (testing "'pop argument 1' command -> codes"
      (is (= pop-argument-1-codes (command->codes {:type "C_POP"
                                                   :name "pop"
                                                   :segment "argument"
                                                   :value "1"
                                                   :path "Sample.vm"}))))
    (testing "'pop temp 6' command -> codes"
      (is (= pop-temp-6-codes (command->codes {:type "C_POP"
                                               :name "pop"
                                               :segment "temp"
                                               :value "6"
                                               :path "Sample.vm"}))))
    (testing "'pop static 1' command -> codes"
      (is (= pop-static-1-codes (command->codes {:type "C_POP"
                                                 :name "pop"
                                                 :segment "static"
                                                 :value "1"
                                                 :path "Sample.vm"}))))))
