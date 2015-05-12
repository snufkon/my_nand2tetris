(ns vm-translator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]
            [vm-translator.file :as file]
            [vm-translator.parser :refer [parse]]
            [vm-translator.code-writer :refer [write]])
  (:gen-class main true))

(defn- vm-files
  [dir-path]
  (->> (file/files dir-path)
       (filter #(= "vm" (file/ext %)))))

(defn- source->asm-file-path
  [source]
  (let [src-dir (file/parent-dir source)
        basename (file/basename source)]
    (str src-dir "/" basename ".asm")))

(defn -main
  [source & args]
  (let [asm-file (source->asm-file-path source)
        files (if (file/dir? source)
                (vm-files source)
                [source])]
    (->> (mapcat parse files)
         (write asm-file))
    (println source "->" asm-file)))
