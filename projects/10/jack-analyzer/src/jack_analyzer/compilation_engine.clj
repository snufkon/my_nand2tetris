(ns jack-analyzer.compilation-engine
  (:refer-clojure :exclude [read])
  (:require [jack-analyzer.tokenizer :as tokenizer]
            [jack-analyzer.xml-writer :as writer]))

(def tokens (atom []))

(defn- current-token
  []
  (first @tokens))

(defn- next-token
  []
  (second @tokens))

(defn- advance!
  []
  (swap! tokens #(rest %)))

(defn- keyword-const?
  [token]
  (if (contains? #{"true" "false" "null" "this"} (:value token))
    true
    false))

(defn- unary-op?
  [token]
  (if (contains? #{"-" "~"} (:value token))
    true
    false))

(defn- term?
  []
  (cond
    (= :int-const (:type (current-token)))    true
    (= :string-const (:type (current-token))) true
    (keyword-const? (current-token))          true
    (= :identifier (:type (current-token)))   true
    (= "(" (:value (current-token)))          true
    (unary-op? (current-token))               true
    :else false))

(defn- accept-keyword
  [value]
  (let [token (current-token)]
    (when (and (= :keyword (:type token))
               (= value (:value token)))
      (writer/write-terminal-tag "keyword" (:value token))
      (advance!))))

(defn- accept-symbol
  [value]
  (let [token (current-token)]
    (when (and (= :symbol (:type token))
               (= value (:value token)))
      (writer/write-terminal-tag "symbol" (:value token))
      (advance!))))

(defn- accept-symbols
  [values]
  (some accept-symbol values))

(defn- accept-int-const
  []
  (let [token (current-token)]
    (when (= :int-const (:type token))
      (writer/write-terminal-tag "integerConstant" (:value token))
      (advance!))))

(defn- accept-string-const
  []
  (let [token (current-token)]
    (when (= :string-const (:type token))
      (writer/write-terminal-tag "stringConstant" (:value token))
      (advance!))))

(defn- accept-identifier
  []
  (let [token (current-token)]
    (when (= :identifier (:type token))
      (writer/write-terminal-tag "identifier" (:value token))
      (advance!))))

(defn- accept-keyword-const
  []
  (let [token (current-token)]
    (when (keyword-const? token)
      (writer/write-terminal-tag "keyword" (:value token))
      (advance!))))

(defn- accept-type
  []
  (let [token (current-token)]
    (cond
      (= :identifier (:type token))
      (do (writer/write-terminal-tag "identifier" (:value token))
          (advance!))
      
      (contains? #{"int" "char" "boolean"} (:value token))
      (do (writer/write-terminal-tag "keyword" (:value token))
          (advance!)))))

(defn- expect-keyword
  [value]
  (when-not (accept-keyword value)
    (throw (ex-info "failed: unexpected keyword"
                    {:expect {:value value :type :keyword}
                     :token (current-token)}))))

(defn- expect-symbol
  [value]
  (when-not (accept-symbol value)
    (throw (ex-info "failed: unexpected symbol"
                    {:expect {:value value :type :keyword}
                     :token (current-token)}))))

(defn- expect-var-name
  []
  (when-not (accept-identifier)
    (throw (ex-info "failed: unexpected var name"
                    {:expect "varName (type = :identifier)"
                     :token (current-token)}))))

(defn- expect-class-name
  []
  (when-not (accept-identifier)
    (throw (ex-info "failed: unexpected class name"
                    {:expect "className (type = :identifier)"
                     :token (current-token)}))))

(defn- expect-subroutine-name
  []
  (when-not (accept-identifier)
    (throw (ex-info "failed: unexpected subroutine name"
                    {:expect "subroutineName (type = :identifier)"
                     :token (current-token)}))))

(defn- expect-type
  []
  (when-not (accept-type)
    (throw (ex-info "failed: unexpected type"
                    {:expect "'int' | 'char' | 'boolean' | className"
                     :token (current-token)}))))


;;;
;;; expression
;;;

(declare compile-term compile-subroutine-call compile-expression-list)

(defn- compile-expression
  "term (op term)*"
  []
  (writer/write-non-terminal-start-tag "expression")
  (compile-term)
  (loop []
    (when (accept-symbols ["+" "-" "*" "/" "&" "|" "<" ">" "="])
      (compile-term)
      (recur)))
  (writer/write-non-terminal-end-tag "expression"))

(defn- compile-term
  "integerConstant | stringConstant | keywordConstant | varName | 
   varName '[' expression ']' | subroutineCall | '(' expression ')' |
   unaryOp term"
  []
  (writer/write-non-terminal-start-tag "term")
  (cond
    (accept-int-const) nil
    (accept-string-const) nil
    (accept-keyword-const) nil

    (= :identifier (:type (current-token)))
    (case (:value (next-token))
      "[" (do
            (expect-var-name)
            (expect-symbol "[")
            (compile-expression)
            (expect-symbol "]"))
      "(" (compile-subroutine-call)
      "." (compile-subroutine-call)
      (expect-var-name))

    (accept-symbol "(")
    (do
      (compile-expression)
      (expect-symbol ")"))

    (unary-op? (current-token))
    (do
      (expect-symbol (:value (current-token)))
      (compile-term))

    :else
    (throw (ex-info "failed: compile-term"
                    {:token (current-token)})))
  (writer/write-non-terminal-end-tag "term"))

(defn- compile-subroutine-call
  "subroutineName '(' expressionList ')' |
   (className | varName) '.' subroutineName '(' expressionList ')'"
  []
  (when (= "." (:value (next-token)))
    (when-not (accept-identifier)
      (throw (ex-info "failed: compile-subroutine-call"
                      {:expect-type :identifier
                       :token (current-token)})))
    (expect-symbol "."))
  (expect-subroutine-name)
  (expect-symbol "(")
  (compile-expression-list)
  (expect-symbol ")"))

(defn- compile-expression-list
  "( expression (',' expression)* )?"
  []
  (writer/write-non-terminal-start-tag "expressionList")
  (when (term?)
    (compile-expression)
    (loop []
      (when (accept-symbol ",")
        (compile-expression)
        (recur))))
  (writer/write-non-terminal-end-tag "expressionList"))


;;;
;;; statement
;;;

(declare compile-let compile-if compile-while compile-do compile-return)

(defn- compile-statements
  "statements: statement*
   statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement"
  []
  (writer/write-non-terminal-start-tag "statements")
  (loop []
    (cond
      (= "let" (:value (current-token)))
      (do (compile-let) (recur))

      (= "do" (:value (current-token)))
      (do (compile-do) (recur))

      (= "return" (:value (current-token)))
      (do (compile-return) (recur))

      (= "if" (:value (current-token)))
      (do (compile-if) (recur))

      (= "while" (:value (current-token)))
      (do (compile-while) (recur))))
  (writer/write-non-terminal-end-tag "statements"))

(defn- compile-let
  "'let' varName ('[' expression ']')? '=' expression ';'"
  []
  (writer/write-non-terminal-start-tag "letStatement")
  (expect-keyword "let")
  (expect-var-name)
  (when (accept-symbol "[")
    (compile-expression)
    (expect-symbol "]"))
  (expect-symbol "=")  
  (compile-expression)
  (expect-symbol ";")  
  (writer/write-non-terminal-end-tag "letStatement"))

(defn- compile-if
  "'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?"
  []
  (writer/write-non-terminal-start-tag "ifStatement")
  (expect-keyword "if")
  (expect-symbol "(")
  (compile-expression)
  (expect-symbol ")")
  (expect-symbol "{")
  (compile-statements)
  (expect-symbol "}")
  (when (accept-keyword "else")
    (expect-symbol "{")
    (compile-statements)
    (expect-symbol "}"))
  (writer/write-non-terminal-end-tag "ifStatement"))

(defn- compile-while
  "'while' '(' expression ')' '{' statements '}'"
  []
  (writer/write-non-terminal-start-tag "whileStatement")
  (expect-keyword "while")
  (expect-symbol "(")
  (compile-expression)
  (expect-symbol ")")
  (expect-symbol "{")
  (compile-statements)
  (expect-symbol "}")
  (writer/write-non-terminal-end-tag "whileStatement"))

(defn- compile-do
  "'do' subroutineCall ';'"
  []
  (writer/write-non-terminal-start-tag "doStatement")  
  (expect-keyword "do")
  (compile-subroutine-call)
  (expect-symbol ";")
  (writer/write-non-terminal-end-tag "doStatement"))

(defn- compile-return
  "'return' expression? ';'"
  []
  (writer/write-non-terminal-start-tag "returnStatement")    
  (expect-keyword "return")
  (when (term?)
    (compile-expression))
  (expect-symbol ";")
  (writer/write-non-terminal-end-tag "returnStatement"))


;;;
;;; class
;;;

(declare compile-class-var-dec compile-subroutine compile-subroutine-body
         compile-parameter-list compile-var-dec)

(defn- compile-class
  "'class' className '{' classVarDec* subroutineDec* '}'"
  []
  (writer/write-non-terminal-start-tag "class" false)
  (expect-keyword "class")
  (expect-class-name)
  (expect-symbol "{")
  (loop []
    (when (contains? #{"static" "field"} (:value (current-token)))
      (compile-class-var-dec)
      (recur)))
  (loop []
    (when (contains? #{"constructor" "function" "method"} (:value (current-token)))
      (compile-subroutine)
      (recur)))
  (expect-symbol "}")
  (writer/write-non-terminal-end-tag "class"))

(defn- compile-class-var-dec
  "('static' | 'field') type varName (',' varName)* ';'"
  []
  (writer/write-non-terminal-start-tag "classVarDec")
  (when-not (or (accept-keyword "static")
                (accept-keyword "field"))
    (throw (ex-info "failed: compile-class-var-dec"
                    {:expect "('static' | 'field')"
                     :token (current-token)})))
  (expect-type)
  (expect-var-name)
  (loop []
    (when (accept-symbol ",")
      (expect-var-name)
      (recur)))
  (expect-symbol ";")
  (writer/write-non-terminal-end-tag "classVarDec"))

(defn- compile-subroutine
  "('constructor' | 'function' | 'method') ('void' | type)
   subroutineName '(' parameterList ')' subroutineBody"
  []
  (writer/write-non-terminal-start-tag "subroutineDec")
  (when-not (or (accept-keyword "constructor")
                (accept-keyword "function")
                (accept-keyword "method"))
    (throw (ex-info "failed: compile-subroutine"
                    {:expect "('constructor' | 'function' | 'method')"
                     :token (current-token)})))
  (when-not (or (accept-keyword "void")
                (accept-keyword "int")
                (accept-keyword "char")
                (accept-keyword "boolean")
                (accept-identifier))
    (throw (ex-info "failed: compile-subroutine"
                    {:expect "('void' | type)"
                     :token (current-token)})))
  (expect-subroutine-name)
  (expect-symbol "(")
  (compile-parameter-list)
  (expect-symbol ")")
  (compile-subroutine-body)
  (writer/write-non-terminal-end-tag "subroutineDec"))

(defn- compile-subroutine-body
  "'{' varDec* statements '}'"
  []
  (writer/write-non-terminal-start-tag "subroutineBody")
  (expect-symbol "{")
  (loop []
    (when (= "var" (:value (current-token)))
      (compile-var-dec)
      (recur)))
  (compile-statements)
  (expect-symbol "}")
  (writer/write-non-terminal-end-tag "subroutineBody"))

(defn- compile-parameter-list
  "((type varName) (',' type varName)*)?"
  []
  (writer/write-non-terminal-start-tag "parameterList")
  (when (accept-type)
    (expect-var-name)
    (loop []
      (when (= "," (:value (current-token)))
        (expect-symbol ",")
        (expect-type)
        (expect-var-name)
        (recur))))
  (writer/write-non-terminal-end-tag "parameterList"))

(defn- compile-var-dec
  "'var' type varName (',' varName)* ';'"
  []
  (writer/write-non-terminal-start-tag "varDec")
  (expect-keyword "var")
  (expect-type)
  (expect-var-name)
  (loop []
    (when (= "," (:value (current-token)))
      (expect-symbol ",")
      (expect-var-name)
      (recur)))
  (expect-symbol ";")
  (writer/write-non-terminal-end-tag "varDec"))


(defn- read
  [ts]
  (reset! tokens ts))

(defn parse
  [ts f]
  (read ts)
  (writer/init f)
  (compile-class))
