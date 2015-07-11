(ns jack-compiler.compilation-engine
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as cstr]
            [jack-compiler
             [symbol-table :as stable]
             [vm-writer :as vm-writer]]))

(def tokens (atom []))

(def class-name (atom ""))

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

(defn- kind->segment
  [kind]
  (case kind
    "STATIC" "static"
    "FIELD"  "this"
    "ARG"    "argument"
    "VAR"    "local"
    nil))

(defn- accept-keyword
  [value]
  (let [token (current-token)]
    (when (and (= :keyword (:type token))
               (= value (:value token)))
      (advance!))))

(defn- accept-symbol
  [value]
  (let [token (current-token)]
    (when (and (= :symbol (:type token))
               (= value (:value token)))
      (advance!))))

(defn- accept-symbols
  [values]
  (some accept-symbol values))

(defn- accept-int-const
  []
  (let [token (current-token)]
    (when (= :int-const (:type token))
      (advance!))))

(defn- accept-string-const
  []
  (let [token (current-token)]
    (when (= :string-const (:type token))
      (advance!))))

(defn- accept-identifier
  []
  (let [token (current-token)]
    (when (= :identifier (:type token))
      (advance!))))

(defn- accept-keyword-const
  []
  (let [token (current-token)]
    (when (keyword-const? token)
      (advance!))))

(defn- accept-type
  []
  (let [token (current-token)]
    (cond
      (= :identifier (:type token))
      (advance!)
      
      (contains? #{"int" "char" "boolean"} (:value token))
      (advance!))))

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
  (let [var-name (:value (current-token))]
    (if (accept-identifier)
      var-name
      (throw (ex-info "failed: unexpected var name"
                      {:expect "varName (type = :identifier)"
                       :token (current-token)})))))

(defn- expect-class-name
  []
  (let [class-name (:value (current-token))]
    (if (accept-identifier)
      class-name
      (throw (ex-info "failed: unexpected class name"
                      {:expect "className (type = :identifier)"
                       :token (current-token)})))))

(defn- expect-subroutine-name
  []
  (let [subroutine-name (:value (current-token))]
    (if (accept-identifier)
      subroutine-name
      (throw (ex-info "failed: unexpected subroutine name"
                      {:expect "subroutineName (type = :identifier)"
                       :token (current-token)})))))

(defn- expect-type
  []
  (let [type (:value current-token)]
    (if (accept-type)
      type
      (throw (ex-info "failed: unexpected type"
                      {:expect "'int' | 'char' | 'boolean' | className"
                       :token (current-token)})))))


;;;
;;; expression
;;;

(declare compile-term compile-subroutine-call compile-expression-list)

(defn- compile-expression
  "term (op term)*"
  []
  (compile-term)
  (loop []
    (cond
      (accept-symbol "+") (do (compile-term)
                              (vm-writer/write-arithmetic "add")
                              (recur))
      (accept-symbol "-") (do (compile-term)
                              (vm-writer/write-arithmetic "sub")
                              (recur))
      (accept-symbol "*") (do (compile-term)
                              (vm-writer/write-call "Math.multiply" 2)
                              (recur))
      (accept-symbol "/") (do (compile-term)
                              (vm-writer/write-call "Math.divide" 2)
                              (recur))
      (accept-symbol "&") (do (compile-term)
                              (vm-writer/write-arithmetic "and")
                              (recur))
      (accept-symbol "|") (do (compile-term)
                              (vm-writer/write-arithmetic "or")
                              (recur))
      (accept-symbol "<") (do (compile-term)
                              (vm-writer/write-arithmetic "lt")
                              (recur))
      (accept-symbol ">") (do (compile-term)
                              (vm-writer/write-arithmetic "gt")
                              (recur))
      (accept-symbol "=") (do (compile-term)
                              (vm-writer/write-arithmetic "eq")
                              (recur)))))

(defn- trim-double-quotation
  [s]
  (-> s
      (subs 1 (-> s count dec))))

(defn- compile-term
  "integerConstant | stringConstant | keywordConstant | varName | 
   varName '[' expression ']' | subroutineCall | '(' expression ')' |
   unaryOp term"
  []
  (let [value (:value (current-token))]
    (cond
      (accept-int-const) (vm-writer/write-push "constant" value)
      (accept-string-const)
      (let [chars (trim-double-quotation value)
            char-num (count chars)]
        (vm-writer/write-push "constant" char-num)
        (vm-writer/write-call "String.new" 1)
        (doseq [c chars]
          (vm-writer/write-push "constant" (int c))
          (vm-writer/write-call "String.appendChar" 2)))

      (or (= "null" value) (= "false" value))
      (do (accept-keyword-const)
          (vm-writer/write-push "constant" 0))

      (= "true" value)
      (do (accept-keyword-const)
          (vm-writer/write-push "constant" 1)
          (vm-writer/write-arithmetic "neg"))

      (= "this" value)
      (do (accept-keyword-const)
          (vm-writer/write-push "pointer" 0))

      (= :identifier (:type (current-token)))
      (case (:value (next-token))
        ;; varName[expression]
        "[" (let [var-name (expect-var-name)
                  index (stable/index-of var-name)
                  kind (stable/kind-of var-name)
                  segment (kind->segment kind)]
              (expect-symbol "[")
              (compile-expression)
              (expect-symbol "]")
              (vm-writer/write-push segment index)
              (vm-writer/write-arithmetic "add")
              (vm-writer/write-pop "pointer" 1)
              (vm-writer/write-push "that" 0))
        "(" (compile-subroutine-call)
        "." (compile-subroutine-call)
        (let [var-name (expect-var-name)
              index (stable/index-of var-name)
              kind (stable/kind-of var-name)
              segment (kind->segment kind)]
          (vm-writer/write-push segment index)))

      (accept-symbol "(")
      (do
        (compile-expression)
        (expect-symbol ")"))

      (unary-op? (current-token))
      (do
        (let [op (:value (current-token))]
          (expect-symbol op)
          (compile-term)
          (case op
            "-" (vm-writer/write-arithmetic "neg")
            "~" (vm-writer/write-arithmetic "not")
            (throw (ex-info "failed: compile-term"
                            {:expect "- | ~"
                             :token (current-token)})))))

      :else
      (throw (ex-info "failed: compile-term"
                      {:token (current-token)})))))

(defn- compile-subroutine-call
  "subroutineName '(' expressionList ')' |
   (className | varName) '.' subroutineName '(' expressionList ')'"
  []

  (if (= "." (:value (next-token)))
    ;; (className|varName).subroutineName(expressionList)
    (let [class-or-var-name (:value (current-token))
          kind (stable/kind-of class-or-var-name)
          type (stable/type-of class-or-var-name)
          index (stable/index-of class-or-var-name)]
      (expect-class-name)
      (expect-symbol ".")
      (let [name (:value (current-token))]
        (expect-subroutine-name)
        (expect-symbol "(")
        (when (not= "NONE" kind)
          (vm-writer/write-push (kind->segment kind) index))
        (let [expression-num (compile-expression-list)]
          (if (= "NONE" kind)
            (vm-writer/write-call (str class-or-var-name "." name)  expression-num)
            (vm-writer/write-call (str type "." name) (inc expression-num))))
        (expect-symbol ")")))
    ;; subroutineName(expressionList)
    (let [name (expect-subroutine-name)]
      (vm-writer/write-push "pointer" 0)
      (expect-symbol "(")
      (let [expression-num (compile-expression-list)
            expression-num (inc expression-num)] ;; argument 0 is used to refer 'this' object
        (vm-writer/write-call (str @class-name "." name) expression-num))
      (expect-symbol ")"))))

(defn- compile-expression-list
  "( expression (',' expression)* )?"
  []
  (let [expression-num (atom 0)]
    (when (term?)
      (compile-expression)
      (swap! expression-num inc)
      (loop []
        (when (accept-symbol ",")
          (compile-expression)
          (swap! expression-num inc)
          (recur))))
    @expression-num))


;;;
;;; statement
;;;

(declare compile-let compile-if compile-while compile-do compile-return)

(defn- compile-statements
  "statements: statement*
   statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement"
  []
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
      (do (compile-while) (recur)))))

(defn- compile-let
  "'let' varName ('[' expression ']')? '=' expression ';'"
  []
  (expect-keyword "let")
  (let [name (expect-var-name)
        index (stable/index-of name)
        segment (-> name stable/kind-of kind->segment)
        let-to-array? (accept-symbol "[")]
    (when let-to-array?
      (compile-expression)
      (expect-symbol "]")
      (vm-writer/write-push segment index)      
      (vm-writer/write-arithmetic "add"))
    (expect-symbol "=")  
    (compile-expression)
    (expect-symbol ";")
    (if let-to-array?
      (do
        (vm-writer/write-pop "temp" 0)
        (vm-writer/write-pop "pointer" 1)
        (vm-writer/write-push "temp" 0)
        (vm-writer/write-pop "that" 0))
      (vm-writer/write-pop segment index))))

(defn- compile-if
  "'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?"
  []
  (expect-keyword "if")
  (expect-symbol "(")
  (compile-expression)
  (expect-symbol ")")
  (let [label-else (-> (gensym "if") str)
        label-end (-> (gensym "if") str)]
    (vm-writer/write-arithmetic "not")
    (vm-writer/write-if label-else)
    (expect-symbol "{")
    (compile-statements)
    (expect-symbol "}")
    (vm-writer/write-goto label-end)
    (vm-writer/write-label label-else)
    (when (accept-keyword "else")
      (expect-symbol "{")
      (compile-statements)
      (expect-symbol "}"))
    (vm-writer/write-label label-end)))


(defn- compile-while
  "'while' '(' expression ')' '{' statements '}'"
  []
  (let [label-start (-> (gensym "while") str)
        label-end (-> (gensym "while") str)]
    (expect-keyword "while")
    (expect-symbol "(")
    (vm-writer/write-label label-start)
    (compile-expression)
    (expect-symbol ")")
    (vm-writer/write-arithmetic "not")
    (vm-writer/write-if label-end)
    (expect-symbol "{")
    (compile-statements)
    (expect-symbol "}")
    (vm-writer/write-goto label-start)
    (vm-writer/write-label label-end)))

(defn- compile-do
  "'do' subroutineCall ';'"
  []
  (expect-keyword "do")
  (compile-subroutine-call)
  (vm-writer/write-pop "temp" 0)
  (expect-symbol ";"))

(defn- compile-return
  "'return' expression? ';'"
  []
  (expect-keyword "return")
  (if (term?)
    (compile-expression)
    (vm-writer/write-push "constant" 0)) ; for 'void' subroutine
  (expect-symbol ";")
  (vm-writer/write-return))


;;;
;;; class
;;;

(declare compile-class-var-dec compile-subroutine
         compile-parameter-list compile-var-dec)

(defn- compile-class
  "'class' className '{' classVarDec* subroutineDec* '}'"
  []
  (expect-keyword "class")
  (let [name (:value (current-token))]
    (expect-class-name)
    (reset! class-name name))
  (expect-symbol "{")
  (loop []
    (when (contains? #{"static" "field"} (:value (current-token)))
      (compile-class-var-dec)
      (recur)))
  (loop []
    (when (contains? #{"constructor" "function" "method"} (:value (current-token)))
      (compile-subroutine)
      (recur)))
  (expect-symbol "}"))

(defn- compile-class-var-dec
  "('static' | 'field') type varName (',' varName)* ';'"
  []
  (let [kind (:value (current-token))]
    (when-not (or (accept-keyword "static")
                  (accept-keyword "field"))
      (throw (ex-info "failed: compile-class-var-dec"
                      {:expect "('static' | 'field')"
                       :token (current-token)})))
    (let [type (:value (current-token))
          name (:value (next-token))]
      (expect-type)
      (expect-var-name)
      (stable/define name type kind))
    (loop []
      (when (accept-symbol ",")
        (let [name (:value (current-token))]
          (expect-var-name)
          (stable/define name type kind))
        (recur))))
  (expect-symbol ";"))

(defn- compile-subroutine
  "('constructor' | 'function' | 'method') ('void' | type)
   subroutineName '(' parameterList ')' '{' varDec* statements '}'"
  []
  (stable/start-subroutine)
  (let [subroutine-type (:value (current-token))]
    (cond
      (accept-keyword "constructor") nil
      (accept-keyword "function")    nil
      (accept-keyword "method")      nil
      
      :else
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

    (when (= "method" subroutine-type)
      (stable/define "this" @class-name "arg"))
    (let [name (expect-subroutine-name)]
      (expect-symbol "(")
      (compile-parameter-list)
      (expect-symbol ")")
      (expect-symbol "{")
      (loop []
        (when (= "var" (:value (current-token)))
          (compile-var-dec)
          (recur)))
      (vm-writer/write-function (str @class-name "." name) (stable/var-count "VAR"))
      (case subroutine-type
        "constructor" (do
                        (vm-writer/write-push "constant" (stable/var-count "FIELD"))
                        (vm-writer/write-call "Memory.alloc" 1)
                        (vm-writer/write-pop "pointer" 0))
        "method" (do
                   (vm-writer/write-push "argument" 0)
                   (vm-writer/write-pop "pointer" 0))
        "function"))
    (compile-statements)
    (expect-symbol "}")))

(defn- compile-parameter-list
  "((type varName) (',' type varName)*)?"
  []
  (let [parameter-num (atom 0)]
    (let [type (:value (current-token))]
      (when (accept-type)
        (let [name (:value (current-token))]
          (expect-var-name)
          (swap! parameter-num inc)
          (stable/define name type "arg"))
        (loop []
          (when (= "," (:value (current-token)))
            (expect-symbol ",")
            (let [type (:value (current-token))
                  name (:value (next-token))]
              (expect-type)
              (expect-var-name)
              (swap! parameter-num inc)
              (stable/define name type "arg"))
            (recur)))))
    @parameter-num))

(defn- compile-var-dec
  "'var' type varName (',' varName)* ';'"
  []
  
  (expect-keyword "var")
  (let [type (:value (current-token))
        name (:value (next-token))]
    (expect-type)
    (expect-var-name)
    (stable/define name type "var")
    (loop []
      (when (= "," (:value (current-token)))
        (expect-symbol ",")
        (let [name (:value (current-token))]
          (expect-var-name)
          (stable/define name type "var"))
        (recur))))
  (expect-symbol ";"))


(defn- read
  [ts]
  (reset! tokens ts))

(defn parse
  [ts f]
  (read ts)
  (stable/init)
  (vm-writer/init)
  (compile-class)
  (spit f (cstr/join "\n" @vm-writer/commands)))
