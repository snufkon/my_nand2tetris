(ns jack-compiler.core
  (:refer-clojure :exclude [compile])
  (:require [jack-compiler.tokenizer :as tokenizer]
            [jack-compiler.compilation-engine :as engine]
            [jack-compiler.file :as file])
  (:gen-class main true))

(defn- jack-files
  [dir-path]
  (->> (file/files dir-path)
       (filter #(= "jack" (file/ext %)))))

(defn- compile
  [jack-file]
  (let [basename (file/basename jack-file)
        parent-dir (file/parent-dir jack-file)
        tokenizer-out-file (str parent-dir "/" basename "T.xml")
        parser-out-file (str parent-dir "/" basename ".vm")
        tokens (tokenizer/read jack-file)]
    ;;(tokenizer/write tokenizer-out-file)
    ;; (println jack-file "->" tokenizer-out-file)
    (engine/parse tokens parser-out-file)
    (println jack-file "->" parser-out-file)))

(defn -main
  [source & args]
  (let [files (if (file/dir? source)
                (jack-files source)
                [source])]
    (doseq [file files]
      (compile file))))
