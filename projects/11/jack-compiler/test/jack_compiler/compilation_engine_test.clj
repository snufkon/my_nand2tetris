(ns jack-compiler.compilation-engine-test
  (:require [clojure.string :as cstr]
            [clojure.test :refer :all]
            [jack-compiler.compilation-engine :as engine]
            [jack-compiler.symbol-table :as stable]
            [jack-compiler.vm-writer :as writer]))

(defn- setup
  [class-name defines]
  (reset! engine/class-name class-name)
  ;;(stable/start-subroutine)
  (stable/init)
  (doseq [define defines]
    (apply stable/define (-> define (cstr/split #" ") reverse))))

(defn- read-code
  [s]
  (writer/init)
  (-> s
      (#'jack-compiler.tokenizer/read-string)
      (#'jack-compiler.compilation-engine/read)))

(defn- tokens-are-all-compiled?
  []
  (= nil (#'jack-compiler.compilation-engine/current-token)))

(defn- replace-labels
  [commands]
  (let [label-map (atom {})
        label-index (atom 1)]
    (for [cmd commands]
      (if-let [[_ name label] (re-find #"^(if-goto|goto|label) (.*)" cmd)]
        (if-let [replaced-label (get @label-map label)]
          (str name " " replaced-label)
          (let [new-label (str "L" @label-index)]
            (swap! label-map assoc label new-label)
            (swap! label-index inc)
            (str name " " new-label)))
        cmd))))

(defn- result-commands
  []
  (replace-labels @writer/commands))


;;;
;;; expression
;;; 

(deftest compile-expression-test
  (testing "with valid expression"
    (read-code "12345")
    (#'jack-compiler.compilation-engine/compile-expression)
    (is (tokens-are-all-compiled?))
    
    (read-code "((y + size) < 254) & ((x + size) < 510)")
    (#'jack-compiler.compilation-engine/compile-expression)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid expression"))


(deftest compile-term-test
  (testing "with valid term"
    (testing "integerConstant"
      (read-code "12345")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "stringConstant"
      (read-code "\"hello world\"")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "keywordConstant"
      (read-code "true")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "varName"
      (read-code "i")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "varName[expression]"
      (read-code "data[0]")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "subroutineCall"
      (read-code "run()")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?))
      
      (read-code "game.run()")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "(expression)"
      (read-code "(x + y)")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?)))
    
    (testing "unaryOp term"
      (read-code "-12345")
      (#'jack-compiler.compilation-engine/compile-term)
      (is (tokens-are-all-compiled?))))
  
  (testing "with invalid term"
    (read-code "do")
    (is (thrown? Exception (#'jack-compiler.compilation-engine/compile-term)))))


(deftest compile-subroutine-call-test
  (testing "with valid subroutine call"
    (testing "subroutineName(expressionList)"
      (read-code "hello(name)")
      (#'jack-compiler.compilation-engine/compile-subroutine-call)
      (is (tokens-are-all-compiled?)))
    
    (testing "(className | varName).subroutineName(expressionList)"
      (read-code "game.run()")
      (#'jack-compiler.compilation-engine/compile-subroutine-call)
      (is (tokens-are-all-compiled?))))
  
  (testing "with invalid subroutine call"
    (read-code "game.run(")
    (is (thrown? Exception (#'jack-compiler.compilation-engine/compile-subroutine-call)))))


(deftest compile-expression-list-test
  (testing "with valid expression list"
    
    (testing "expression"
      (read-code "i")
      (is (= 1 (#'jack-compiler.compilation-engine/compile-expression-list)))
      (is (tokens-are-all-compiled?)))
    (testing "expression, expression"
      (read-code "i, j")
      (is (= 2 (#'jack-compiler.compilation-engine/compile-expression-list))) 
      (is (tokens-are-all-compiled?)))

    (testing "expression, expression, expression, expression"
      (read-code "x, y, x, y")
      (is (= 4 (#'jack-compiler.compilation-engine/compile-expression-list))) 
      (is (tokens-are-all-compiled?)))
    
    (testing "empty"
      (read-code "")
      (is (= 0 (#'jack-compiler.compilation-engine/compile-expression-list)))
      (is (tokens-are-all-compiled?))))
  
  (testing "with invalid expression list"
    (read-code "i,")
    (is (thrown? Exception (#'jack-compiler.compilation-engine/compile-expression-list)))))


;;;
;;; statement
;;;

(deftest compile-let-test
  (testing "with valid 'let' statement"
    (testing "let varName = expression;"
      (setup "LetTest" ["var int i"])
      (read-code "let i=0;")
      (#'jack-compiler.compilation-engine/compile-let)
      (is (tokens-are-all-compiled?))
      (is (= ["push constant 0"
              "pop local 0"]
             (result-commands))))
    
    (testing "let varName[expression] = expression;"
      (setup "LetTest" ["var Array data"])
      (read-code "let data[3] = 2;")
      (#'jack-compiler.compilation-engine/compile-let)
      (is (tokens-are-all-compiled?))
      (is (= ["push constant 3"
              "push local 0"
              "add"
              "push constant 2"
              "pop temp 0"
              "pop pointer 1"
              "push temp 0"
              "pop that 0"]
             (result-commands)))

      (setup "LetTest" ["var Array a"
                        "var Array b"
                        "var Array c"])
      (read-code "let b[a[3]] = 10;")
      (#'jack-compiler.compilation-engine/compile-let)
      (is (tokens-are-all-compiled?))
      (is (= ["push constant 3"
              "push local 0"
              "add"
              "pop pointer 1"
              "push that 0"
              "push local 1"
              "add"
              "push constant 10"
              "pop temp 0"
              "pop pointer 1"
              "push temp 0"
              "pop that 0"]
             (result-commands)))))
  (testing "with invalid 'let' statement"))


(deftest compile-if-test
  (testing "with valid 'if' statement"
    (setup "IfTest" ["var int x"])
    (read-code
     "if (x) {
       do erase();
      }")
    (#'jack-compiler.compilation-engine/compile-if)
    (is (tokens-are-all-compiled?))
    (is (= ["push local 0"
            "not"
            "if-goto L1"
            "push pointer 0"
            "call IfTest.erase 1"
            "pop temp 0"
            "goto L2"
            "label L1"
            "label L2"] (result-commands)))

    ;; (setup "IfTest" ["var int x"
    ;;                  "var int size"])
    (read-code
     "if (x) {
        do erase();
        let size = size;
        do draw();
      } else {
        do erase();
      }")
    (#'jack-compiler.compilation-engine/compile-if)
    (is (tokens-are-all-compiled?))
    ;; (is (= ["push local 0"
    ;;         "not"
    ;;         "if-goto L1"
    ;;         "call IfTest.erase 1"
    ;;         ""]))

    (read-code
     "if (((y + size) < 254) & ((x + size) < 510)) {
        do erase();
        let size = size + 2;
        do draw();
      }")
    (#'jack-compiler.compilation-engine/compile-if)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid 'if' statement"))


(deftest compile-while-test
  (testing "with valid 'while' statement"
    (setup "WhileTest" ["var int i"])
    (read-code
     "while (i < 5) {
        do draw();
        let i = i + 1;
      }")
    (#'jack-compiler.compilation-engine/compile-while)
    (is (tokens-are-all-compiled?))
    (is (= ["label L1"
            "push local 0"
            "push constant 5"
            "lt"
            "not"
            "if-goto L2"
            "push pointer 0"
            "call WhileTest.draw 1"
            "pop temp 0"
            "push local 0"
            "push constant 1"
            "add"
            "pop local 0"
            "goto L1"
            "label L2"] (result-commands)))
  (testing "with invalid 'while' statement")))


(deftest compile-do-test
  (testing "with valid 'do' statement"
    (testing "with no argument"
      (read-code "do Output.println();")
      (#'jack-compiler.compilation-engine/compile-do)
      (is (tokens-are-all-compiled?))
      (is (= ["call Output.println 0"
              "pop temp 0"]
             (result-commands))))

    (testing "with argument"
      (setup "DoTest" ["var int x"
                       "var int y"])
      (read-code "do Screen.drawRectangle(x, y, x, y);")
      (#'jack-compiler.compilation-engine/compile-do)
      (is (tokens-are-all-compiled?))
      (is (= ["push local 0"
              "push local 1"
              "push local 0"
              "push local 1"
              "call Screen.drawRectangle 4"
              "pop temp 0"] (result-commands))))

    (testing "method call"
      (println "start test ========================================")
      (setup "DoTest" ["var String s"])
      (read-code "do s.setInt(10);")
      (#'jack-compiler.compilation-engine/compile-do)
      (is (tokens-are-all-compiled?))
      (is (= ["push local 0"
              "push constant 10"
              "call String.setInt 2"
              "pop temp 0"]
             (result-commands))))
    

    
    )
  (testing "with invalid 'do' statement"))


(deftest compile-return-test
  (testing "with valid 'return' statement"
    (read-code "return;")
    (#'jack-compiler.compilation-engine/compile-return)
    (is (tokens-are-all-compiled?))
    (is (= ["push constant 0"
            "return"]
           (result-commands)))
    
    (read-code "return 10;")
    (#'jack-compiler.compilation-engine/compile-return)
    (is (tokens-are-all-compiled?))
    (is (= ["push constant 10"
            "return"]
           (result-commands))))
  (testing "with invalid 'return' statement"))


;;;
;;; class
;;;

(deftest compile-class-test
  (testing "with valid class"
    (read-code "class Square {}")
    (#'jack-compiler.compilation-engine/compile-class)
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
    (#'jack-compiler.compilation-engine/compile-class)
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
    (#'jack-compiler.compilation-engine/compile-class)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid class"))


(deftest compile-class-var-dec
  (testing "with valid class var declaration"
    (read-code "field int size;")
    (#'jack-compiler.compilation-engine/compile-class-var-dec)
    (is (tokens-are-all-compiled?))

    (read-code "field int x, y;")
    (#'jack-compiler.compilation-engine/compile-class-var-dec)
    (is (tokens-are-all-compiled?)))
  (testing "with invalid class var declaration"))


(deftest compile-subroutine-test
  (testing "with valid subroutine"
    (setup "SubroutineTest" [])
    (read-code
     "function void main() {
        var SquareGame game;
      }")
    (#'jack-compiler.compilation-engine/compile-subroutine)
    (is (tokens-are-all-compiled?))
    (is (= ["function SubroutineTest.main 1"] (result-commands)))

    (read-code
     "method void draw() {
        do Screen.setColor(x);
        // do Screen.drawRectangle(x, y, x, y);
        return;
      }")
    (#'jack-compiler.compilation-engine/compile-subroutine)
    (is (tokens-are-all-compiled?))

    (setup "SubroutineTest" ["field int x"])
    (read-code
     "constructor SubroutineTest new() {
        let x = 1;
        return this;
      }")
    (#'jack-compiler.compilation-engine/compile-subroutine)
    (is (tokens-are-all-compiled?))
    (is (= ["function SubroutineTest.new 0"
            "push constant 1"
            "call Memory.alloc 1"
            "pop pointer 0"
            "push constant 1"
            "pop this 0"
            "push pointer 0"
            "return"]
           (result-commands))))
  
  (testing "with invalid subroutine"))


(deftest compile-parameter-list-test
  (testing "with valid parameter list"
    (read-code "int i")
    (is (= 1 (#'jack-compiler.compilation-engine/compile-parameter-list))) 
    (is (tokens-are-all-compiled?))

    (read-code "int i, boolean result")
    (is (= 2 (#'jack-compiler.compilation-engine/compile-parameter-list))) 
    (is (tokens-are-all-compiled?))

    (read-code "int i, boolean result, MyClass cls")
    (is (= 3 (#'jack-compiler.compilation-engine/compile-parameter-list))) 
    (is (tokens-are-all-compiled?)))

  (testing "with invalid parameter list"
    (read-code "int char")
    (is (thrown? Exception (#'jack-compiler.compilation-engine/compile-parameter-list)))))


(deftest compile-var-dec-test
  (testing "with valid 'var' declaration"
    (read-code "var int i;")
    (#'jack-compiler.compilation-engine/compile-var-dec)
    (is (tokens-are-all-compiled?))

    (read-code "var int i, j, k;")
    (#'jack-compiler.compilation-engine/compile-var-dec)
    (is (tokens-are-all-compiled?)))
  
  (testing "with invalid 'var' declaration"
    (read-code "var int i")
    (is (thrown? Exception (#'jack-compiler.compilation-engine/compile-var-dec)))))

