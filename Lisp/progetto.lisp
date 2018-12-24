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
        'NOFLAG
        'FLAG)))

; SOTTRAZIONE
(defun sottrazione (Acc Pointer Mem)
    (cons (mod (- Acc (nth Pointer Mem)) 1000)
    (if (>= (- Acc (nth Pointer Mem)) 0) 
        'NOFLAG
        'FLAG)))

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
    (if (and (= Acc 0) (equal Flag 'NOFLAG))
        Pointer
        (mod (+ Pc 1) 100)))

; BRANCH IF POSITIVE
(defun branch-if-positive (Pc Pointer Flag)
    (if (equal Flag 'NOFLAG)
        Pointer
        (mod (+ Pc 1) 100)))

; INPUT
(defun input (In)
    (cons (first In) (rest In)))

; OUTPUT
(defun output (Acc Out) 
    (append Out (list Acc)))
  
; ONE INSTRUCTION
(defun one-instruction (state)
    (let 
        (
            (ACC (nth 2 state))
            (PC (nth 4 state))
            (MEM (nth 6 state))
            (IN (nth 8 state))
            (OUT (nth 10 state))
            (FLAG (nth 12 state))
        )
            (let 
                (
                    (ISTR (nth PC MEM))
                )
                (let ((POINTER (mod ISTR 100))) 
                (cond ((and (>= ISTR 0) (< ISTR 100)) 
                        (list 'halted-state ':acc ACC ':pc PC ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((and (> ISTR 99) (< ISTR 200)) 
                        (list 'state ':acc (car (addizione ACC POINTER MEM)) ':pc (+ PC 1) ':mem MEM ':in IN ':out OUT ':flag (cdr (addizione ACC POINTER MEM)) ))
                      ((and (> ISTR 199) (< ISTR 300)) 
                        (list 'state ':acc (car (sottrazione ACC POINTER MEM)) ':pc (+ PC 1) ':mem MEM ':in IN ':out OUT ':flag (cdr (sottrazione ACC POINTER MEM)) ))
                      ((and (> ISTR 299) (< ISTR 400)) 
                        (list 'state ':acc ACC ':pc (+ PC 1) ':mem (store ACC POINTER MEM) ':in IN ':out OUT ':flag FLAG ))
                      ((and (> ISTR 399) (< ISTR 500)) 
                        (list 'halted-state ':acc ACC ':pc PC ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((and (> ISTR 499) (< ISTR 600)) 
                        (list 'state ':acc (lload POINTER MEM) ':pc (+ PC 1) ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((and (> ISTR 599) (< ISTR 700)) 
                        (list 'state ':acc ACC ':pc (branch POINTER) ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((and (> ISTR 699) (< ISTR 800)) 
                        (list 'state ':acc ACC ':pc (branch-if-zero PC ACC POINTER FLAG) ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((and (> ISTR 799) (< ISTR 900)) 
                        (list 'state ':acc ACC ':pc (branch-if-positive PC POINTER FLAG) ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((= ISTR 900) 
                        (list 'halted-state ':acc ACC ':pc PC ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((and (= ISTR 901) (= (list-length IN) 0)) 
                        (list 'halted-state ':acc ACC ':pc PC ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                      ((= ISTR 901) 
                        (list 'state ':acc (car (input IN)) ':pc (+ PC 1) ':mem MEM ':in (cdr (input IN)) ':out OUT ':flag FLAG ))
                      ((= ISTR 902) 
                        (list 'state ':acc ACC ':pc (+ PC 1) ':mem MEM ':in IN ':out (output ACC OUT) ':flag FLAG ))
                      ((and (> ISTR 902) (< ISTR 1000)) 
                        (list 'halted-state ':acc ACC ':pc PC ':mem MEM ':in IN ':out OUT ':flag FLAG ))
                )
            
            )
        )
    )
)

(defun execution-loop (state)
    (cond ((and (< (nth 4 state) 100) (equal (nth 0 state) 'state)) (execution-loop (one-instruction state)))
          ((equal (nth 0 state) 'halted-state) (nth 10 state))
    )   
)

(defun split (chars str &optional (lst nil) (accm ""))
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
   (split chars (subseq str 1) (cons accm lst) "")
   (split chars (subseq str 1) 
                        lst 
                        (concatenate 'string
           accm
         (string c))))
            ))))




; PARSER

(defun remove-comment (row) 
    (string-trim " " (subseq row 0 (search "//" row))))
