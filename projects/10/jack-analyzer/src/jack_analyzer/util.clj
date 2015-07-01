(ns jack-analyzer.util
  (:require [clojure.string :as cstr]))

(defn escape-pattern
  [s]
  (java.util.regex.Pattern/quote s))

(defn escape-html
  [s]
  (cstr/escape s {\< "&lt;"
                  \> "&gt;"
                  \& "&amp;"
                  \" "&quot;"
                  \' "^#39;"}))
