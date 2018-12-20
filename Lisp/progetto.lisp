; UTILS

; Replace
(defun repl (list n elem) 
 (if (= n 0) 
    (append (list elem) (cdr list))
    (append (list (car list)) (repl (cdr list) (- n 1) elem))))


; ISTRUZIONI

; ADDIZIONE
(defun addizione (Acc Pointer Mem)
    (cons (mod (+ Acc (nth Pointer Mem)) 1000)
    (if (< (+ Acc (nth Pointer Mem)) 1000) 
        "noflag"
        "flag")))

; SOTTRAZIONE
(defun sottrazione (Acc Pointer Mem)
    (cons (mod (- Acc (nth Pointer Mem)) 1000)
    (if (>= (- Acc (nth Pointer Mem)) 0) 
        "noflag"
        "flag")))

; STORE
(defun store (Acc Pointer Mem)
    (repl Mem Pointer Acc))

; LOAD
(defun lload (Pointer Mem) 
    (nth Pointer Mem))

; BRANCH
(defun branch (Pc) (+ Pc 0)) 

; BRANCH IF ZERO
(defun branch-if-zero (Pc Acc Pointer Flag)
    (if (and (= Acc 0) (equal Flag "noflag"))
        Pointer
        (mod (+ Pc 1) 100)))

; BRANCH IF POSITIVE
(defun branch-if-positive (Pc Pointer Flag)
    (if (equal Flag "noflag")
        Pointer
        (mod (+ Pc 1) 100)))

; INPUT
(defun input (In)
    (cons (first In) (rest In)))

; OUTPUT
(defun output (Acc Out) 
    (append Out (list Acc)))

