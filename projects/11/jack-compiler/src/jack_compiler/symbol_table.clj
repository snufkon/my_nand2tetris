(ns jack-compiler.symbol-table
  (:require [clojure.string :as cstr]))

(def table (atom {}))

(def init-counter-value {"static" 0
                         "field"  0
                         "arg"    0
                         "var"    0})
(def counter (atom init-counter-value))

(defn- inc-counter
  [kind]
  (swap! counter update kind inc))

(defn define
  [name type kind]
  (swap! table assoc name {:type type
                           :kind (cstr/upper-case kind)
                           :index (get @counter kind)})
  (inc-counter kind))

(defn kind-of
  [name]
  (-> (get @table name)
      (get :kind "NONE")))

(defn type-of
  [name]
  (-> (get @table name)
      :type))


(defn index-of
  [name]
  (-> (get @table name)
      :index))

(defn var-count
  [kind]
  (-> (filter (fn [[k v]]
                (= kind (:kind v)))
              @table)
      count))

(defn start-subroutine
  []
  (swap! table (fn [t]
                 (->> (for [[k v] t
                            :when (or (= "STATIC" (:kind v))
                                      (= "FIELD" (:kind v)))]
                        [k v])
                      (into {}))))
  (swap! counter #(merge % {"arg" 0 "var" 0})))

(defn init
  []
  (reset! table {})
  (reset! counter init-counter-value))
