COMPOSIZIONE DEL GRUPPO

- Cozzi Davide 829827
- De Rosa Gabriele 829835

DESCRIZIONE

Il file lmc.lisp rappresenta una versione del Little Man Computer 
sviluppata in linguaggio funzionale.


PREDICATI
Documentati nei commenti presenti sul lmc.lisp

    UTILS
    - repl 
    - is-in-list
    - list-parse-integer
    
    PREDICATI DEL SIMULATORE
    Permettono l'esecuzione delle istruzioni presenti in memoria 
    attraverso il passaggio tra i vari stati

    - addizione
    - sottrazione
    - store
    - lload
    - branch
    - branch-if-zero
    - branch-if-positive
    - input
    - output
    - one-instruction
    - checklist
    - execution-loop

    PREDICATI DEL PARSER
    Preso un file di testo sorgente contenente istruzioni assembly
    del Little Man Computer genera la memoria.

    - list-of-zero
    - remove-comment
    - split-core
    - split
    - remove-blank
    - normalize
    - get-value-of
    - value-of
    - command-to-instr
    - read-all-lines-helper
    - read-all-lines
    - is-istr 
    - get-tags
    - get-mem
    - commands-to-mem 
    - lmc-load

    PREDICATI DI ESECUZIONE
    Fa generare la memoria alle istruzioni del parser e la utilizza 
    per generare lo stato iniziale dal quale il simulatore inizia
    l'esecuzione        

    - lmc-run
