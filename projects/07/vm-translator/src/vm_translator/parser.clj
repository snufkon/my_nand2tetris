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
      "add"  "C_ARITHMETIC"
      "sub"  "C_ARITHMETIC"
      "neg"  "C_ARITHMETIC"
      "eq"   "C_ARITHMETIC"
      "gt"   "C_ARITHMETIC"
      "lt"   "C_ARITHMETIC"
      "and"  "C_ARITHMETIC"
      "or"   "C_ARITHMETIC"
      "not"  "C_ARITHMETIC"
      "push" "C_PUSH"
      "pop"  "C_POP")))

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

(defn parse
  [vm-file]
  (->> vm-file
       slurp
       cstr/split-lines
       remove-empty-and-comment-lines
       (map #(line->command % vm-file))))
