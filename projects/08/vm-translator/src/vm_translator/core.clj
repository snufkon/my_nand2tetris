(ns vm-translator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]
            [vm-translator.file :as file]
            [vm-translator.parser :refer [parse]]
            [vm-translator.code-writer :as writer])
  (:gen-class main true))

(defn- vm-files
  [dir-path]
  (->> (file/files dir-path)
       (filter #(= "vm" (file/ext %)))))

(defn- source->asm-file-path
  [source]
  (let [basename (file/basename source)]
    (if (file/dir? source)
      (str source "/" basename ".asm")
      (let [parent-dir (file/parent-dir source)]
        (str parent-dir "/" basename ".asm")))))

(defn -main
  [source & args]
  (writer/init)
  (let [asm-file (source->asm-file-path source)
        files (if (file/dir? source)
                (vm-files source)
                [source])]
    (->> (mapcat parse files)
         (writer/write asm-file))
    (println source "->" asm-file)))

;; (let [;; f "../ProgramFlow/BasicLoop/BasicLoop.vm"
;;       ;; f "../ProgramFlow/FibonacciSeries/FibonacciSeries.vm"
;;       ;; f "../FunctionCalls/SimpleFunction/SimpleFunction.vm"
;;       f "../FunctionCalls/FibonacciElement"
;;       ;;f "../FunctionCalls/StaticsTest"
;;       ;; f "../FunctionCalls/NestedCall"
;;       ]
;;   (-main f))
