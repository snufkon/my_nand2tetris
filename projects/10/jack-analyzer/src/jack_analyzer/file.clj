(ns jack-analyzer.file
  (:require [clojure.java.io :as io]))

(defn dir?
  [path]
  (.isDirectory (io/file path)))

(defn ext
  [path]
  (->> (.getName (io/file path))
      (re-find #"\.(.*$)")
      second))

(defn basename
  [path]
  (if (dir? path)
    (.getName (io/file path))
    (->> (.getName (io/file path))
         (re-find #"(^.*)\..*$")
         second)))

(defn files
  [dir-path]
  (->> (io/file dir-path)
      file-seq
      (map #(.getPath %))))

(defn parent-dir
  [path]
  (.getParent (io/file path)))

