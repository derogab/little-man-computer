COMPOSIZIONE DEL GRUPPO

- Cozzi Davide 829827
- De Rosa Gabriele 829835

DESCRIZIONE

Il file lmc.pl rappresenta una versione del Little Man Computer 
sviluppata in linguaggio logico.

PREDICATI
Documentati nei commenti presenti sul lmc.pl

    PREDICATI DEL SIMULATORE
    Permettono l'esecuzione delle istruzioni presenti in memoria 
    attraverso il passaggio tra i vari stati

    - addizione
    - sottrazione
    - store
    - load
    - branch
    - branchifzero
    - branchifpositive
    - input
    - output
    - instr_in_mem
    - extract_pointer
    - one_instruction
    - checklist
    - execution_loop

    PREDICATI DEL PARSER
    Preso un file di testo sorgente contenente istruzioni assembly
    del Little Man Computer genera la memoria.

    - remove_comment
    - del_blank
    - no_instr
    - exec
    - normalize
    - single_command
    - command
    - command_with_label
    - replace
    - row_to_mem
    - save_labels
    - no_comment
    - memg
    - mem_to_number
    - lmc_load

    PREDICATI DI ESECUZIONE
    Fa generare la memoria alle istruzioni del parser e la utilizza 
    per generare lo stato iniziale dal quale il simulatore inizia
    l'esecuzione        

    - lmc_run
