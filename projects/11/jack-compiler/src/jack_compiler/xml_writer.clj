(ns jack-compiler.xml-writer
  (:refer-clojure :exclude [symbol?])
  (:require [jack-compiler.util :refer [escape-html]]))

(def indent-size 2)
(def current-indent (atom 0))
(def write-file (atom nil))
(def indent-mode? (atom true))

(defn- indent!
  []
  (when @indent-mode?
    (swap! current-indent #(+ % indent-size))))

(defn- unindent!
  []
  (when @indent-mode?
    (swap! current-indent #(- % indent-size))))

(defn- add-indent-to-str
  [s]
  (let [indent (->> (repeat " ")
                    (take @current-indent)
                    (apply str))]
    (str indent s)))

(defn write-non-terminal-start-tag
  ([name]
   (write-non-terminal-start-tag name true))
  ([name append]
   (when @write-file
     (let [tag (-> (str "<" name ">\n")
                   add-indent-to-str)]
       (spit @write-file tag :append append)
       (indent!)))))

(defn write-non-terminal-end-tag
  [name]
  (unindent!)
  (when @write-file
    (let [tag (-> (str "</" name ">\n")
                  add-indent-to-str)]
      (spit @write-file tag :append true))))


(defn- string-constant?
  [name]
  (= name "stringConstant"))

(defn- symbol?
  [name]
  (= name "symbol"))

(defn- escape-string
  [content]
  (-> content
      (subs 1 (-> content count dec))
      escape-html))

(defn- escape-symbol
  [content]
  (escape-html content))

(defn write-terminal-tag
  [name content]
  (when @write-file
    (let [start-tag (str "<" name ">")
          end-tag (str "</" name ">")
          content (cond
                    (string-constant? name) (escape-string content)
                    (symbol? name) (escape-symbol content)
                    :else content)
          tag (-> (str start-tag " " content " " end-tag "\n")
                  add-indent-to-str)]
      (spit @write-file tag :append true))))

(defn init
  ([wf]
   (init wf true))
  ([wf indent?]
   (reset! write-file wf)
   (reset! indent-mode? indent?)))
