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


; PARSER
(defparameter tags (list ))
(defparameter pc 0)

(defun remove-comment (row) 
    (string-trim " " (subseq row 0 (search "//" row))))


(defun split-core (str index) 
    (cond ((= (length str) 0) (list str)) 
          ((>= index (length str)) (list str)) 
          ((equal (char str index) #\ ) (append (list (subseq str 0 index)) (split-core (subseq str (+ index 1)) 0) ))
          (T (split-core str (+ index 1))) ))

(defun split (str) 
    (split-core str 0))

(defun remove-blank (lista)
    (cond ( (= (list-length lista) 0) NIL)
          ((equal (car lista) "") (remove-blank (cdr lista))) 
          (T  (cons (car lista) (remove-blank (cdr lista)) )) ))

(defun normalize (num)
    (cond ((= (length num ) 3) num) 
          ((= (length num ) 2) num)
          ((= (length num ) 1) (concatenate 'string "0" num)) 
          ((= (length num ) 0) "000"))   
)

(defun get-value-core (tag tags) 
    (cond ( (equal tags NIL) NIL )
          ( (equal tag (first (first tags))) (cdr (first tags)) )
          ( T (get-value-core tag (cdr tags)) ) ) )

(defun get-value-of (tag) 
    (get-value-core tag tags))

(defun exec (command) 
    (cond ( (and (equal (string-upcase (car command)) "ADD") (not (equal (cdr command) NIL))) (concatenate 'string "1" (normalize (second command))) )    
          ( (and (equal (string-upcase (car command)) "SUB") (not (equal (cdr command) NIL))) (concatenate 'string "2" (normalize (second command))) )
          ( (and (equal (string-upcase (car command)) "STA") (not (equal (cdr command) NIL))) (concatenate 'string "3" (normalize (second command))) )
          ( (and (equal (string-upcase (car command)) "LDA") (not (equal (cdr command) NIL))) (concatenate 'string "5" (normalize (second command))) )
          ( (and (equal (string-upcase (car command)) "BRA") (not (equal (cdr command) NIL))) (concatenate 'string "6" (normalize (second command))) )
          ( (and (equal (string-upcase (car command)) "BRZ") (not (equal (cdr command) NIL))) (concatenate 'string "7" (normalize (second command))) )
          ( (and (equal (string-upcase (car command)) "BRP") (not (equal (cdr command) NIL))) (concatenate 'string "8" (normalize (second command))) )
          ( (and (equal (string-upcase (car command)) "INP") (equal (cdr command) NIL)) "901" )
          ( (and (equal (string-upcase (car command)) "OUT") (equal (cdr command) NIL)) "902" )
          ( (and (equal (string-upcase (car command)) "HLT") (equal (cdr command) NIL)) "000" )
          ( (equal (string-upcase (car command)) "DAT") (normalize (cdr command)) ) ; TODO: Funzionalità DAT
          ( T (car (cons (exec (cdr command)) (push (cons (string-upcase (car command)) pc) tags) )) )
    ))