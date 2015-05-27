(ns vm-translator.parser
  (:require [clojure.string :as cstr]))

(defn- comment-line?
  [line]
  (if (re-find #"^\s*//" line)
    true
    false))

(defn- empty-line?
  [line]
  (empty? line))

(defn- remove-empty-and-comment-lines
  [lines]
  (remove #(or (empty-line? %) (comment-line? %)) lines))

(defn command-type
  [line]
  (let [name (-> (cstr/split line #" ") first)]
    (case name
      "add"      "C_ARITHMETIC"
      "sub"      "C_ARITHMETIC"
      "neg"      "C_ARITHMETIC"
      "eq"       "C_ARITHMETIC"
      "gt"       "C_ARITHMETIC"
      "lt"       "C_ARITHMETIC"
      "and"      "C_ARITHMETIC"
      "or"       "C_ARITHMETIC"
      "not"      "C_ARITHMETIC"
      "push"     "C_PUSH"
      "pop"      "C_POP"
      "label"    "C_LABEL"
      "goto"     "C_GOTO"
      "if-goto"  "C_IF"
      "function" "C_FUNCTION"
      "call"     "C_CALL"
      "return"   "C_RETURN")))

(defmulti line->command
  (fn [line path]
    (let [type (command-type line)]
      type)))

(defmethod line->command "C_ARITHMETIC"
  [line path]
  (let [name (-> (cstr/split line #" ") first)]
    {:type "C_ARITHMETIC"
     :name name}))

(defmethod line->command "C_PUSH"
  [line path]
  (let [[name segment value] (cstr/split line #" ")]
    {:type "C_PUSH"
     :name name
     :segment segment
     :value value
     :path path}))

(defmethod line->command "C_POP"
  [line path]
  (let [[name segment value] (cstr/split line #" ")]
    {:type "C_POP"
     :name name
     :segment segment
     :value value
     :path path}))

(defmethod line->command "C_LABEL"
  [line path]
  (let [[name label] (cstr/split line #" ")]
    {:type "C_LABEL"
     :name name
     :label label
     :path path}))

(defmethod line->command "C_GOTO"
  [line path]
  (let [[name label] (cstr/split line #" ")]
    {:type "C_GOTO"
     :name name
     :label label
     :path path}))

(defmethod line->command "C_IF"
  [line path]
  (let [[name label] (cstr/split line #" ")]
    {:type "C_IF"
     :name name
     :label label
     :path path}))

(defmethod line->command "C_FUNCTION"
  [line path]
  (let [[name func-name local-var-num] (cstr/split line #" ")]
    {:type "C_FUNCTION"
     :name name
     :func-name func-name
     :local-var-num local-var-num
     :path path}))

(defmethod line->command "C_CALL"
  [line path]
  (let [[name func-name arg-num] (cstr/split line #" ")]
    {:type "C_CALL"
     :name name
     :func-name func-name
     :arg-num arg-num
     :path path}))

(defmethod line->command "C_RETURN"
  [line path]
  (let [name (-> (cstr/split line #" ") first)]
    {:type "C_RETURN"
     :name name
     :path path}))

(defn parse
  [vm-file]
  (->> vm-file
       slurp
       cstr/split-lines
       remove-empty-and-comment-lines
       (map #(line->command % vm-file))))
