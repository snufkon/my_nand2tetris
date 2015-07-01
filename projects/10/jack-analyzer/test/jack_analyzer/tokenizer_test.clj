(ns jack-analyzer.tokenizer-test
  (:require [clojure.test :refer :all]
            [jack-analyzer.tokenizer :refer :all]))

(def s1 "// one line comment")
(def s2 "(+ 1 2) // result is 3")
(def s3 "(+ 1 2) // result is 3\n(+ 1 3) // result is 4")
(def s4 "/* multiline\ncomment */")
(def s5 "(+ 1 2) /* result\nis 3 */")

(deftest remove-one-line-comments-test
  (is (= (#'jack-analyzer.tokenizer/remove-one-line-comments s1) ""))
  (is (= (#'jack-analyzer.tokenizer/remove-one-line-comments s2) "(+ 1 2) "))
  (is (= (#'jack-analyzer.tokenizer/remove-one-line-comments s3) "(+ 1 2) (+ 1 3) ")))

(deftest remove-multiline-comments-test
  (is (= (#'jack-analyzer.tokenizer/remove-multiline-comments s1) s1))
  (is (= (#'jack-analyzer.tokenizer/remove-multiline-comments s4) ""))
  (is (= (#'jack-analyzer.tokenizer/remove-multiline-comments s5) "(+ 1 2) ")))
