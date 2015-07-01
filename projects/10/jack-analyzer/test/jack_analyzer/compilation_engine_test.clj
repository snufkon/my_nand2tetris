(ns jack-analyzer.compilation-engine-test
  (:require [clojure.test :refer :all]))

(defn- read-code
  [s]
  (-> s
      (#'jack-analyzer.tokenizer/read-string)
      (#'jack-analyzer.compilation-engine/read)))

(defn- tokens-are-all-compiled?
  []
  (= nil (#'jack-analyzer.compilation-engine/current-token)))


;;;
;;; expression
;;; 

(deftest compile-expression-test
  (testing "with valid expression"
    (read-code "12345")
    (#'jack-analyzer.compilation-engine/compile-expression)
    (is (tokens-are-all-compiled?))
    
    (read-code "((y + size) < 254) & ((x + size) < 510)")
    (#'jack-analyzer.compilation-engine/compile-expression)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid expression"))


(deftest compile-term-test
  (testing "with valid term"
    (testing "integerConstant"
      (read-code "12345")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "stringConstant"
      (read-code "\"hello world\"")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "keywordConstant"
      (read-code "true")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "varName"
      (read-code "i")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "varName[expression]"
      (read-code "data[0]")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "subroutineCall"
      (read-code "run()")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?))
      
      (read-code "game.run()")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "(expression)"
      (read-code "(x + y)")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "unaryOp term"
      (read-code "-12345")
      (#'jack-analyzer.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?))))
  
  (testing "with invalid term"
    (read-code "do")
    (is (thrown? Exception (#'jack-analyzer.compilation-engine/compile-term)))))


(deftest compile-subroutine-call-test
  (testing "with valid subroutine call"
    (testing "subroutineName(expressionList)"
      (read-code "hello(name)")
      (#'jack-analyzer.compilation-engine/compile-subroutine-call)
      (is (tokens-are-all-compiled?)))
    
    (testing "(className | varName).subroutineName(expressionList)"
      (read-code "game.run()")
      (#'jack-analyzer.compilation-engine/compile-subroutine-call)
      (is (tokens-are-all-compiled?))))
  
  (testing "with invalid subroutine call"
    (read-code "game.run(")
    (is (thrown? Exception (#'jack-analyzer.compilation-engine/compile-subroutine-call)))))


(deftest compile-expression-list-test
  (testing "with valid expression list"
    
    (testing "expression"
      (read-code "i")
      (#'jack-analyzer.compilation-engine/compile-expression-list)
      (is (tokens-are-all-compiled?)))
    (testing "expression, expression"
      (read-code "i, j")
      (#'jack-analyzer.compilation-engine/compile-expression-list)
      (is (tokens-are-all-compiled?)))

    (testing "expression, expression, expression, expression"
      (read-code "x, y, x, y")
      (#'jack-analyzer.compilation-engine/compile-expression-list)
      (is (tokens-are-all-compiled?)))
    
    (testing "empty"
      (read-code "")
      (#'jack-analyzer.compilation-engine/compile-expression-list)
      (is (tokens-are-all-compiled?))))
  
  (testing "with invalid expression list"
    (read-code "i,")
    (is (thrown? Exception (#'jack-analyzer.compilation-engine/compile-expression-list)))))


;;;
;;; statement
;;;

(deftest compile-let-test
  (testing "with valid 'let' statement"
    (testing "let varName = expression;"
      (read-code "let i=0;")
      (#'jack-analyzer.compilation-engine/compile-let)
      (is (tokens-are-all-compiled?)))
    
    (testing "let varName[expression] = expression;"
      (read-code "let data[i] = 10;")
      (#'jack-analyzer.compilation-engine/compile-let)
      (is (tokens-are-all-compiled?))))
  (testing "with invalid 'let' statement"))


(deftest compile-if-test
  (testing "with valid 'if' statement"
    (read-code
     "if (x) {
       do erase();
      }")
    (#'jack-analyzer.compilation-engine/compile-if)
    (is (tokens-are-all-compiled?))

    (read-code
     "if (x) {
        do erase();
        let size = size;
        do draw();
      } else {
        do erase();
      }")
    (#'jack-analyzer.compilation-engine/compile-if)
    (is (tokens-are-all-compiled?))

    (read-code
     "if (((y + size) < 254) & ((x + size) < 510)) {
        do erase();
        let size = size + 2;
        do draw();
      }")
    (#'jack-analyzer.compilation-engine/compile-if)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid 'if' statement"))


(deftest compile-while-test
  (testing "with valid 'while' statement"
    (read-code
     "while (key) {
        let key = key;
        do moveSquare();
      }")
    (#'jack-analyzer.compilation-engine/compile-while)
    (is (tokens-are-all-compiled?))
  (testing "with invalid 'while' statement")))


(deftest compile-do-test
  (testing "with valid 'do' statement"
    (read-code "do game.run();")
    (#'jack-analyzer.compilation-engine/compile-do)
    (is (tokens-are-all-compiled?))
    
    (read-code "do Screen.drawRectangle(x, y, x, y);")
    (#'jack-analyzer.compilation-engine/compile-do)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid 'do' statement"))


(deftest compile-return-test
  (testing "with valid 'return' statement"
    (read-code "return result;")
    (#'jack-analyzer.compilation-engine/compile-return)
    (is (tokens-are-all-compiled?))
    
    (read-code "return;")
    (#'jack-analyzer.compilation-engine/compile-return)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid 'return' statement"))


;;;
;;; class
;;;

(deftest compile-class-test
  (testing "with valid class"
    (read-code "class Square {}")
    (#'jack-analyzer.compilation-engine/compile-class)
    (is (tokens-are-all-compiled?))

    (read-code
     "class Square {
        constructor Square new(int Ax, int Ay, int Asize) {
          let x = Ax;
          let y = Ay;
          let size = Asize;
          
          do draw();
          
          return x; 
        }
      }")
    (#'jack-analyzer.compilation-engine/compile-class)
    (is (tokens-are-all-compiled?))

    (read-code
     "class Square {
        constructor Square new(int Ax, int Ay, int Asize) {
          let x = Ax;
          let y = Ay;
          let size = Asize;
          
          do draw();
          
          return x; 
        }

        method void dispose() {
          do Memory.deAlloc();
          return;
        }
      }")
    (#'jack-analyzer.compilation-engine/compile-class)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid class"))


(deftest compile-class-var-dec
  (testing "with valid class var declaration"
    (read-code "field int size;")
    (#'jack-analyzer.compilation-engine/compile-class-var-dec)
    (is (tokens-are-all-compiled?))

    (read-code "field int x, y;")
    (#'jack-analyzer.compilation-engine/compile-class-var-dec)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid class var declaration"))


(deftest compile-subroutine-test
  (testing "with valid subroutine"
    (read-code
     "function void main() {
        var SquareGame game;
      }")
    (#'jack-analyzer.compilation-engine/compile-subroutine)
    (is (tokens-are-all-compiled?))

    (read-code
     "method void draw() {
        do Screen.setColor(x);
        // do Screen.drawRectangle(x, y, x, y);
        return;
      }")
    (#'jack-analyzer.compilation-engine/compile-subroutine)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid subroutine"))


(deftest compile-parameter-list-test
  (testing "with valid parameter list"
    (read-code "int i")
    (#'jack-analyzer.compilation-engine/compile-parameter-list)
    (is (tokens-are-all-compiled?))

    (read-code "int i, boolean result")
    (#'jack-analyzer.compilation-engine/compile-parameter-list)
    (is (tokens-are-all-compiled?))

    (read-code "int i, boolean result, MyClass cls")
    (#'jack-analyzer.compilation-engine/compile-parameter-list)
    (is (tokens-are-all-compiled?)))

  (testing "with invalid parameter list"
    (read-code "int char")
    (is (thrown? Exception (#'jack-analyzer.compilation-engine/compile-parameter-list)))))


(deftest compile-var-dec-test
  (testing "with valid 'var' declaration"
    (read-code "var int i;")
    (#'jack-analyzer.compilation-engine/compile-var-dec)
    (is (tokens-are-all-compiled?))

    (read-code "var int i, j, k;")
    (#'jack-analyzer.compilation-engine/compile-var-dec)
    (is (tokens-are-all-compiled?)))
  
  (testing "with invalid 'var' declaration"
    (read-code "var int i")
    (is (thrown? Exception (#'jack-analyzer.compilation-engine/compile-var-dec)))))
