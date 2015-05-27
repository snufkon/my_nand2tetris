(ns vm-translator.code-writer
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [vm-translator.file :as file]))

(def current-function (atom ""))
(def return-address-counter (atom 0))

(defn- generate-return-address
  []
  (let [return-address (str "return-address" @return-address-counter)]
    (swap! return-address-counter inc)
    return-address))

(def end-codes ["(END)" "@END" "0;JMP"])

(def pop-and-set-into-a-register-codes
  ["@SP" "M=M-1" "A=M"])

(def pop-and-set-into-d-register-codes
  ["@SP" "M=M-1" "A=M" "D=M"])

(def push-d-register-value-codes
  ["@SP" "A=M" "M=D"])

(def push-true-codes
  ["@SP" "A=M" "M=-1" "@SP" "M=M+1"])

(def push-false-codes
  ["@SP" "A=M" "M=0" "@SP" "M=M+1"])

(def increment-stack-pointer-codes
  ["@SP" "M=M+1"])

(defn load-value-to-d-register-codes
  [value]
  [(str "@" value) "D=A"])

(defn- label->a-inst-code
  [label]
  (str "@" label))

(defn- label->label-symbol-code
  [label]
  (str "(" label ")"))


(defmulti command->codes (fn [command]
                           (-> (:name command) keyword)))

(defmethod command->codes :add
  [_]
  (concat pop-and-set-into-d-register-codes
          pop-and-set-into-a-register-codes
          ["M=M+D"]
          increment-stack-pointer-codes))

(defmethod command->codes :sub
  [_]
  (concat pop-and-set-into-d-register-codes
          pop-and-set-into-a-register-codes
          ["M=M-D"]
          increment-stack-pointer-codes))

(defmethod command->codes :neg
  [_]
  (concat pop-and-set-into-a-register-codes
          ["M=-M"]
          increment-stack-pointer-codes))

(defmethod command->codes :eq
  [_]
  (let [eq-label (-> (gensym "EQ") name)
        eq-end-label (-> (gensym "EQ_END") name)]
    (concat pop-and-set-into-d-register-codes
            pop-and-set-into-a-register-codes
            ["D=M-D"
             (label->a-inst-code eq-label)
             "D;JEQ"]
            push-false-codes
            [(label->a-inst-code eq-end-label)
             "0;JMP"]
            [(label->label-symbol-code eq-label)]
            push-true-codes
            [(label->label-symbol-code eq-end-label)])))

(defmethod command->codes :gt
  [_]
  (let [gt-label (-> (gensym "GT") name)
        gt-end-label (-> (gensym "GT_END") name)]
    (concat pop-and-set-into-d-register-codes
            pop-and-set-into-a-register-codes
            ["D=M-D"
             (label->a-inst-code gt-label)
             "D;JGT"]
            push-false-codes
            [(label->a-inst-code gt-end-label)
             "0;JMP"]            
            [(label->label-symbol-code gt-label)]
            push-true-codes
            [(label->label-symbol-code gt-end-label)])))

(defmethod command->codes :lt
  [_]
  (let [lt-label (-> (gensym "LT") name)
        lt-end-label (-> (gensym "LT_END") name)]
    (concat pop-and-set-into-d-register-codes
            pop-and-set-into-a-register-codes
            ["D=M-D"
             (label->a-inst-code lt-label)             
             "D;JLT"]
            push-false-codes
            [(label->a-inst-code lt-end-label)
             "0;JMP"]
            [(label->label-symbol-code lt-label)]
            push-true-codes
            [(label->label-symbol-code lt-end-label)])))

(defmethod command->codes :and
  [_]
  (concat pop-and-set-into-d-register-codes
          pop-and-set-into-a-register-codes
          ["M=M&D"]
          increment-stack-pointer-codes))

(defmethod command->codes :or
  [_]
  (concat pop-and-set-into-d-register-codes
          pop-and-set-into-a-register-codes
          ["M=M|D"]
          increment-stack-pointer-codes))

(defmethod command->codes :not
  [_]
  (concat pop-and-set-into-a-register-codes
          ["M=!M"]
          increment-stack-pointer-codes))

(defn- static-symbol
  [path value]
  (str (file/basename path) "." value))

(defn- load-segment-value-to-d-register-codes
  [segment value path]
  (let [base (case segment
               "local"    ["@LCL"  "A=M+D" "D=M"]
               "argument" ["@ARG"  "A=M+D" "D=M"]
               "this"     ["@THIS" "A=M+D" "D=M"]
               "that"     ["@THAT" "A=M+D" "D=M"]
               "pointer"  ["@R3"   "A=A+D" "D=M"]
               "temp"     ["@R5"   "A=A+D" "D=M"]
               "static"   [(label->a-inst-code (static-symbol path value)) "D=M"]
               "constant" [])]
    (cond-> []
      (not= "static" segment) (concat (load-value-to-d-register-codes value))
      true (concat base))))

(defmethod command->codes :push
  [{:keys [segment value path]}]
  (concat (load-segment-value-to-d-register-codes segment value path)
          push-d-register-value-codes
          increment-stack-pointer-codes))

(defmethod command->codes :pop
  [{:keys [segment value path]}]
  (let [base (case segment
               "local"    ["@LCL"  "D=M+D"]
               "argument" ["@ARG"  "D=M+D"]
               "this"     ["@THIS" "D=M+D"]
               "that"     ["@THAT" "D=M+D"]
               "pointer"  ["@R3"   "D=A+D"]
               "temp"     ["@R5"   "D=A+D"]
               "static"   [(label->a-inst-code (static-symbol path value)) "M=D"])]
    (if (= "static" segment)
      (concat pop-and-set-into-d-register-codes
              base)
      (concat (load-value-to-d-register-codes value)
              base
              ["@R13" "M=D"]
              pop-and-set-into-d-register-codes
              ["@R13" "A=M" "M=D"]))))

(defn- label->current-function-label
  [label]
  (str @current-function "$" label))

(defmethod command->codes :label
  [{:keys [label]}]
  [(-> label label->current-function-label label->label-symbol-code)])

(defmethod command->codes :goto
  [{:keys [label]}]
  (let [label-name (str @current-function "$" label)]
    [(-> label label->current-function-label label->a-inst-code)
     "0;JMP"]))

(defmethod command->codes :if-goto
  [{:keys [label]}]
  (concat pop-and-set-into-d-register-codes
          [(-> label label->current-function-label label->a-inst-code)
           "D;JNE"]))

(defmethod command->codes :function
  [{:keys [func-name local-var-num]}]
  (let [loop-label (str func-name "_LOOP")
        loop-end-label (str func-name "_END")]
    (reset! current-function func-name)
    [(label->label-symbol-code func-name)
     (str "@" local-var-num)
     "D=A"
     (label->label-symbol-code loop-label)
     (label->a-inst-code loop-end-label)
     "D;JEQ"
     "D=D-1"
     "@SP"
     "A=M"
     "M=0"
     "@SP"
     "M=M+1"
     (label->a-inst-code loop-label)
     "0;JMP"
     (label->label-symbol-code loop-end-label)]))

(defmethod command->codes :call
  [{:keys [func-name arg-num]}]
  (let [return-address (generate-return-address)]
    (concat [(label->a-inst-code return-address)"D=A"]
            push-d-register-value-codes
            increment-stack-pointer-codes
            ;; push LCL
            ["@LCL" "D=M"]
            push-d-register-value-codes
            increment-stack-pointer-codes
            ;; push ARG
            ["@ARG" "D=M"]
            push-d-register-value-codes
            increment-stack-pointer-codes
            ;; push THIS
            ["@THIS" "D=M"]
            push-d-register-value-codes
            increment-stack-pointer-codes
            ;; push THAT
            ["@THAT" "D=M"]
            push-d-register-value-codes
            increment-stack-pointer-codes
            [;; ARG = SP - (n+5)
             (label->a-inst-code arg-num)
             "D=A"
             "@5"
             "D=A+D"
             "@SP"
             "D=M-D"
             "@ARG"
             "M=D"
             ;; LCL = SP
             "@SP"
             "D=M"
             "@LCL"
             "M=D"
             ;; goto func-name
             (label->a-inst-code func-name)
             "0;JMP"
             ;; return-address
             (label->label-symbol-code return-address)])))

(defn- restore-caller-base-address-codes
  [symbol]
  (let [offset (case symbol
                 "THAT" 1
                 "THIS" 2
                 "ARG"  3
                 "LCL"  4)]
    [(label->a-inst-code offset)
     "D=A"
     "@R13"
     "A=M-D"
     "D=M"
     (label->a-inst-code symbol)
     "M=D"]))

(defmethod command->codes :return
  [_]
  (concat [;; FRAME = LCL, use R13 as a FRAME
           "@LCL"
           "D=M"
           "@R13"
           "M=D"
           ;; RET = *(FRAME-5), use R14 as a FRAME
           "@5"
           "D=A"
           "@R13"
           "A=M-D"
           "D=M"
           "@R14"
           "M=D"
           ;; *ARG = pop()
           "@SP"
           "M=M-1"
           "A=M"
           "D=M"
           "@ARG"
           "A=M"
           "M=D"
           ;; SP = ARG + 1
           "@ARG"
           "D=M+1"
           "@SP"
           "M=D"]
          ;; THAT = *(FRAME-1)
          (restore-caller-base-address-codes "THAT")
          ;; THIS = *(FRAME-2)
          (restore-caller-base-address-codes "THIS")
          ;; ARG  = *(FRAME-3)
          (restore-caller-base-address-codes "ARG")
          ;; LCL  = *(FRAME-4)
          (restore-caller-base-address-codes "LCL")
          ;; goto RET
          ["@R14"
           "A=M"
           "0;JMP"]))

(defn- make-init-codes
  []
  (concat ["@256" "D=A" "@SP" "M=D"]
          (command->codes {:type "C_CALL"
                           :name "call"
                           :func-name "Sys.init"
                           :arg-num "0"})))

(defn write
  [asm-file commands]
  (let [init-codes (make-init-codes)
        main-codes (mapcat command->codes commands)
        ;; use for FibonacciElement, NestedCall and StaticsTest
        codes (concat init-codes main-codes)
        ;; use for BasicLoop, FibonacciSeries and Simple Function
        ;; codes (concat main-codes end-codes)
        ]
    (->> codes
         (cstr/join "\n")
         (spit asm-file))))

(defn init
  [& {:keys [function-name counter]
      :or {function-name "" counter 0}}]
  (reset! current-function function-name)
  (reset! return-address-counter counter))
