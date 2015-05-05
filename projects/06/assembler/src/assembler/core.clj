 (ns assembler.core
  (:require [clojure.string :as cstr]
            [assembler.parser :as parser]
            [assembler.code :as code]
            [assembler.symbol-table :as stable])
  (:gen-class main true))

(defn- ->binary-string
  [s]
  (let [bs (if (integer? s)
             (Integer/toBinaryString s)
             (-> s Integer/parseInt Integer/toBinaryString))
        padding-num (- 16 (count bs))]
    (str (apply str (repeat padding-num "0")) bs)))

(defn extract-command
  [line]
  (->> line
      (re-find #"^\s*([^/\s]+)")
      second))

(defn- extract-commands
  [lines]
  (keep extract-command lines))

(defn- load-assembly-program
  [f]
  (-> f slurp cstr/split-lines))

(defn- write-codes-to-file
  [f codes]
  (spit f (cstr/join "\n" codes)))

(defn- symbol-is-number?
  [sym]
  (try
    (when (Integer/parseInt sym)
      true)
  (catch NumberFormatException e
    false)))

(defmulti command->code (fn [command]
                          (parser/command-type command)))

(defmethod command->code "A_COMMAND"
  [command]
  (let [sym (parser/command-symbol command)]
    (if (symbol-is-number? sym)
      (->binary-string sym)
      (do
        (when-not (stable/symbol-contains? sym)
          (stable/add-new-value sym))
        (-> (stable/get-address sym) ->binary-string)))))

(defmethod command->code "C_COMMAND"
  [command]
  (let [comp-code (-> command parser/comp-mnemonic code/comp-mnemonic->code)
        dest-code (-> command parser/dest-mnemonic code/dest-mnemonic->code)
        jump-code (-> command parser/jump-mnemonic code/jump-mnemonic->code)]
    (str "111" comp-code dest-code jump-code)))

(defn- parse-first-time
  [commands]
  (loop [cmds commands i 0 parsed-commands []]
    (if (empty? cmds)
      parsed-commands
      (let [cmd (first cmds)]
        (if (= (parser/command-type cmd) "L_COMMAND")
          (do
            (stable/add-entry (parser/command-symbol cmd) i)
            (recur (rest cmds) i parsed-commands))
          (recur (rest cmds) (inc i) (conj parsed-commands cmd)))))))

(defn- parse-second-time
  [commands]
  (map command->code commands))

(defn -main
  [f & args]
  (stable/init-table)
  (let [codes (-> f
                  load-assembly-program
                  extract-commands
                  parse-first-time
                  parse-second-time)
        wf (cstr/replace f ".asm" ".hack")]
    (write-codes-to-file wf codes)
    (println f " -> " wf)))
