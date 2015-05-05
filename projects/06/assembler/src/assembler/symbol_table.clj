(ns assembler.symbol-table)

(def table (atom {}))
(def current-address (atom nil))

(defn init-table
  []
  (reset! table {"SP"         0
                 "LCL"        1
                 "ARG"        2
                 "THIS"       3
                 "THAT"       4
                 "R0"         0
                 "R1"         1
                 "R2"         2 
                 "R3"         3
                 "R4"         4
                 "R5"         5
                 "R6"         6
                 "R7"         7
                 "R8"         8
                 "R9"         9
                 "R10"       10
                 "R11"       11
                 "R12"       12
                 "R13"       13
                 "R14"       14
                 "R15"       15
                 "SCREEN" 16384
                 "KBD"    24576})
  (reset! current-address 16))

(defn add-entry
  [symbol address]
  (swap! table assoc symbol address))

(defn add-new-value
  [symbol]
  (add-entry symbol @current-address)
  (swap! current-address inc))

(defn symbol-contains?
  [symbol]
  (if (get @table symbol)
    true
    false))

(defn get-address
  [symbol]
  (get @table symbol))
