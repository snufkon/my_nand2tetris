(ns jack-compiler.vm-writer)

(def ^:private write-file (atom nil))

(def commands (atom []))

(defn- add-command!
  [cmd]
  (swap! commands conj cmd))

(defn write-push
  [segment index]
  (let [cmd (str "push " segment " " index)]
    (add-command! cmd)))

(defn write-pop
  [segment index]
  (let [cmd (str "pop " segment " " index)]
    (add-command! cmd)))

(defn write-arithmetic
  [command]
  (add-command! command))

(defn write-label
  [label]
  (let [cmd (str "label " label)]
    (add-command! cmd)))

(defn write-goto
  [label]
  (let [cmd (str "goto " label)]
    (add-command! cmd)))

(defn write-if
  [label]
  (let [cmd (str "if-goto " label)]
    (add-command! cmd)))

(defn write-call
  [name num-of-args]
  (let [cmd (str "call " name " " num-of-args)]
    (add-command! cmd)))

(defn write-function
  [name num-of-locals]
  (let [cmd (str "function " name " " num-of-locals)]
    (add-command! cmd)))

(defn write-return
  []
  (add-command! "return"))

(defn init
  []
  (reset! commands []))
