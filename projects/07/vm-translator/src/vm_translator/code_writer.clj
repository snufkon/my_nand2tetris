(ns vm-translator.code-writer
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [vm-translator.file :as file]))

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


(defn write
  [asm-file commands]
  (let [codes (-> (mapcat command->codes commands)
                  (concat end-codes))]
    (->> codes
         (cstr/join "\n")
         (spit asm-file))))
