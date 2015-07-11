(ns jack-compiler.tokenizer
  (:refer-clojure :exclude [read symbol? keyword? read-string])
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [jack-compiler.xml-writer :as writer]
            [jack-compiler.util :refer [escape-pattern escape-html]]))

(def tokens (atom []))

(defn- remove-one-line-comments
  [s]
  (->> (cstr/split-lines s)
      (map #(cstr/replace % #"//.*" ""))
      (apply str)))

(defn- remove-multiline-comments
  [s]
  (cstr/replace s #"(?s)\/\*.*?\*\/" ""))


;;;
;;; keyword
;;; 

(def keywords ["class" "constructor" "function" "method" "field" "static"
               "var" "int" "char" "boolean" "void" "true" "false" "null"
               "this" "let" "do" "if" "else" "while" "return"])

(def keywords-pattern-str (->> keywords
                               (map #(str % "(?!\\w)"))
                               (cstr/join "|")))

(defn keyword?
  [s]
  (contains? (set keywords) s))


;;;
;;; symbol
;;;

(def symbols ["{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&"
              "|" "<" ">" "=" "~"])

(def symbols-pattern-str (cstr/join "|" (map escape-pattern symbols)))

(defn symbol?
  [s]
  (contains? (set symbols) s))


;;;
;;; integerConstant
;;; 

(def integer-constant-pattern-str "\\d+")

(defn integer-constant?
  [s]
  (let [pattern-str (str "^" integer-constant-pattern-str "$")
        pattern (re-pattern pattern-str)]
    (if (re-find pattern s)
      true
      false)))


;;;
;;; stringConstant
;;; 

(def string-constant-pattern-str "\".*?\"")

(defn string-constant?
  [s]
  (let [pattern-str (str "^" string-constant-pattern-str "$")
        pattern (re-pattern pattern-str)]
    (if (re-find pattern s)
      true
      false)))


;;;
;;; identifier
;;;

(def identifier-pattern-str "[^0-9\\s]\\w*")

(defn identifier?
  [s]
  (let [pattern-str (str "^" identifier-pattern-str "$")
        pattern (re-pattern pattern-str)]
    (if (re-find pattern s)
      true
      false)))

(def terminal-pattern
  (->> [keywords-pattern-str symbols-pattern-str integer-constant-pattern-str
        string-constant-pattern-str identifier-pattern-str]
       (cstr/join "|")
       re-pattern))

(defn- token-type
  [value]
  (cond
    (keyword? value)          :keyword
    (symbol? value)           :symbol
    (integer-constant? value) :int-const
    (string-constant? value)  :string-const
    (identifier? value)       :identifier
    :else (-> "unknown token type" Exception. throw)))

(defrecord Token [value type])

(defn new-token
  [value]
  (map->Token {:value value :type (token-type value)}))

(defn- read-string
  [s]
  (let [code (-> s
                 remove-multiline-comments
                 remove-one-line-comments)
        matcher (re-matcher terminal-pattern code)]
    (loop [read-tokens []]
      (if-let [result (re-find matcher)]
        (recur (conj read-tokens (new-token result)))
        (reset! tokens read-tokens)))))

(defn read
  [file]
  (-> file slurp read-string))

(defn write
  [f]
  (writer/init f false)
  (writer/write-non-terminal-start-tag "tokens")
  (doseq [token @tokens
          :let [name (case (:type token)
                       :keyword "keyword"
                       :symbol "symbol"
                       :int-const "integerConstant"
                       :string-const "stringConstant"
                       :identifier "identifier")]]
    (writer/write-terminal-tag name (:value token)))
  (writer/write-non-terminal-end-tag "tokens"))
