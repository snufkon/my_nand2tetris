(ns assembler.parser)

(defn comment?
  [line]
  (if (re-find #"^//" line)
    true
    false))

(defn command-type
  [command]
  (case (first command)
    \@  "A_COMMAND"
    \(  "L_COMMAND"
        "C_COMMAND"))

(defn command-symbol
  [command]
  (case (command-type command)
    "A_COMMAND" (subs command 1) 
    "L_COMMAND" (subs command 1 (dec (count command)))))

(defn dest-mnemonic
  [command]
  (let [result (re-find #"(.*)=" command)]
    (if result
      (second result)
      "null")))

(defn comp-mnemonic
  [command]
  (if (= (dest-mnemonic command) "null")
    (-> (re-find #"(.*);" command) second)
    (-> (re-find #"=(.*)" command) second)))

(defn jump-mnemonic
  [command]
  (let [result (re-find #";(.*)" command)]
    (if result
      (second result)
      "null")))
