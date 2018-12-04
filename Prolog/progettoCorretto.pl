% ISTRUZIONI IN MEMORIA

/**
 * Addizione
 * 
 * Instruction: 1xx 
 * 
 * Somma il contenuto della cella di memoria xx con il valore contenuto 
 * nell'accumulatore e scrive il valore risultante nell'accumulatore. 
 * Il valore salvato nell'accumulatore è la somma modulo 1000. 
 * Se la somma non supera 1000 il flag è impostato ad assente, 
 * se invece raggiunge o supera 1000 il flag è impostato a presente.
 *
 *
 * MEMO: Pointer contiene il valore già tranciato 
 * MEMO: Il risultato dovrà essere il nuovo accumulatore
*/
addizione(Acc, Pointer, Mem, X, flag) :- nth0(Pointer, Mem, Value, _),
                                         Y is Acc+Value,
                                         X is ((Acc+Value)mod 1000),
                                         Y > 999,
                                         !.
addizione(Acc, Pointer, Mem, X, noflag) :- nth0(Pointer, Mem, Value, _),
                                           Y is Acc+Value,
                                           X is ((Acc+Value)mod 1000),
                                           Y < 1000.

/**
 * Sottrazione
 * 
 * Instruction: 2xx
 *
 * Sottrae il contenuto della cella di memoria xx dal valore contenuto 
 * nell'accumulatore e scrive il valore risultante nell'accumulatore. 
 * Il valore salvato nell'accumulatore è la differenza modulo 1000. 
 * Se la differenza è inferiore a zero il flag è impostato a presente, 
 * se invece è positiva o zero il flag è impostato ad assente.
 *
 *
 * MEMO: Il risultato dovrà essere il nuovo accumulatore
*/
sottrazione(Acc, Pointer, Mem, X, flag) :- nth0(Pointer, Mem, Value, _),
                                           Y is Acc-Value,
                                           X is ((Acc-Value)mod 1000),
                                           Y < 0,
                                           !.
sottrazione(Acc, Pointer, Mem, X, noflag) :- nth0(Pointer, Mem, Value, _),
                                             Y is Acc-Value,
                                             X is ((Acc-Value)mod 1000),
                                             Y >= 0.

/**
 * Store
 * 
 * Instruction: 3xx
 *
 * Salva il valore contenuto nell'accumulatore nella cella di memoria 
 * avente indirizzo xx. 
 * Il contenuto dell'accumulatore rimane invariato.
*/
store(Acc, Pointer, MemIn, MemOut) :- nth0(Pointer, MemIn, _, Varmem),
                                      nth0(Pointer, MemOut, Acc, Varmem).

/**
 * Instruction not found
 * 
 * Instruction: 4xx
 *
 * I numeri tra 400 e 499 non hanno un corrispettivo.
 * Corrispondono a delle istruzioni non valide (illegal instructions). 
 * Si fermerà con una condizione di errore.
*/
notfound(Pointer) :- write(Pointer), write(": illegal instruction").

/**
 * Load
 * 
 * Instruction: 5xx
 * 
 * Scrive il valore contenuto nella cella di memoria di indirizzo xx nell'accumulatore. 
 * Il contenuto della cella di memoria rimane invariato.
*/
load(Acc, Pointer, MemIn) :- nth0(Pointer, MemIn, Acc, _).

/**
 * Branch
 * 
 * Instruction: 6xx
 *
 * Salto non condizionale. 
 * Imposta il valore del program counter a xx.
 *
 * branch(Pc, NewPointer4Pc) / 2
*/
branch(Pc, Pc).

/**
 * Branch if zero
 * 
 * Instruction: 7xx
 *
 * Salto condizionale.
 * Imposta il valore del program counter a xx solamente se 
 * il contenuto dell'accumulatore è zero e se il flag è assente.
 *
 * branchifzero(Pc, Acc, Pointer, Flag, NewPc) / 5
*/
branchifzero(_, 0, Pointer, noflag, Pointer) :- !.
branchifzero(Pc, _, _, noflag, NewPc) :- NewPc is (Pc + 1).
branchifzero(Pc, _, _, flag, NewPc) :- NewPc is (Pc + 1).

/*branchifzero(Pc, Acc, Pointer, Flag, NewPc) :- (Acc = 0, Flag = noflag)->
                                               NewPc is Pointer;
                                               NewPc is Pc+1.*/

/**
 * Branch if positive
 * 
 * Instruction: 8xx
 *
 * Salto condizionale. 
 * Imposta il valore del program counter a xx solamente se il flag è assente.
 *
 * branchifpositive(Pc, Pointer, Flag, NewPc) / 4
*/
branchifpositive(_, Pointer, noflag, Pointer) :- !.
branchifpositive(Pc, _, flag, NewPc) :- NewPc is (Pc + 1).

/**
 * Input
 * 
 * Instruction: 901
 *
 * Scrive il contenuto presente nella testa della coda in input 
 * nell'accumulatore e lo rimuove dalla coda di input.
*/
input(Acc, [Acc|NewQueueIn], NewQueueIn).

/**
 * Output
 * 
 * Instruction: 902
 *
 * Scrive il contenuto dell'accumulatore alla fine della coda di output. 
 * Il contenuto dell'accumulatore rimane invariato.
*/
output(Acc, QueueOut, NewQueueOut) :- append(QueueOut, [Acc], NewQueueOut).

/**
 * Halt
 * 
 * Instruction: 0xx
 *
 * Termina l'esecuzione del programma. 
 * Nessuna ulteriore istruzione viene eseguita.
*/
lmc_halt :- halt.



% LITTLE MAN COMPUTER

/**
 * Istruzione in memoria
 *
 * Estraggo l'istruzione in memoria puntata dal Program Counter
 *
 * instr_in_mem(Pc, Mem, Instr) / 3
*/
instr_in_mem(Pc, Mem, Instruction) :- nth0(Pc, Mem, Instruction, _).

/**
 * Puntatore nell'istruzione
 *
 * Estraggo il puntatore in memoria dall'istruzione
 * Esempio: da Ixx tengo xx
*/
extract_pointer(I, X) :- X is (I mod 100).

/**
 * One Instruction
 *
 * Esegui una istruzione:
 * - Leggi in memoria l'istruzione puntata dal PC
 * - Estrai i dati dall'istruzione
 * - Controllo l'istruzione da eseguire
 * - Eseguo
*/
one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag),
    halted_state(Acc, Pc, Mem, In, Out, Flag)

) :- instr_in_mem(Pc, Mem , Istr),
     Istr >= 0,
     Istr < 100,
     !.

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag),
    halted_state(Acc, Pc, Mem, In, Out, Flag)

) :- instr_in_mem(Pc, Mem , Istr),
     Istr >= 400,
     Istr < 500,
     !.

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag),
    halted_state(Acc, Pc, Mem, In, Out, Flag)

) :- instr_in_mem(Pc, Mem , Istr),
     Istr >= 900,
     Istr \= 901,
     Istr \= 902,
     !.

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr >= 100,
     Istr < 200,
     addizione(Acc, Pointer, Mem, Acc2, Flag2),
     Pc2 is Pc+1,
     append([], Mem, Mem2),
     append([], In, In2),
     append([], Out, Out2).

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr >= 200,
     Istr < 300,
     sottrazione(Acc, Pointer, Mem, Acc2, Flag2),
     Pc2 is Pc+1,
     append([], Mem, Mem2),
     append([], In, In2),
     append([], Out, Out2).


one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr >= 300,
     Istr < 400,
     store(Acc, Pointer, Mem, Mem2),
     Acc2 is Acc,
     Pc2 is Pc+1,
     append([], In, In2),
     append([], Out, Out2),
     copy_term(Flag, Flag2).

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr >= 500,
     Istr < 600,
     load(Acc2, Pointer, Mem),                                   
     Pc2 is Pc+1,
     append([], Mem, Mem2),
     append([], In, In2),
     append([], Out, Out2),
     copy_term(Flag, Flag2).

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr >= 600,
     Istr < 700,
     branch(Pc2, Pointer),
     Acc2 is Acc,
     append([], Mem, Mem2),
     append([], In, In2),
     append([], Out, Out2),
     copy_term(Flag, Flag2).

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr >= 700,
     Istr < 800,
     branchifzero(Pc, Acc, Pointer, Flag, Pc2),
     Acc2 is Acc,
     append([], Mem, Mem2),
     append([], In, In2),
     append([], Out, Out2),
     copy_term(Flag, Flag2).


one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr >= 800,
     Istr < 900,
     branchifpositive(Pc, Pointer, Flag, Pc2),
     Acc2 is Acc,
     append([], Mem, Mem2),
     append([], In, In2),
     append([], Out, Out2),
     copy_term(Flag, Flag2).

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr = 901,
     proper_length(In, InEmpty),
     InEmpty \= 0,
     input(Acc2, In, In2),
     Pc2 is Pc+1,
     append([], Mem, Mem2),
     append([], Out, Out2),
     copy_term(Flag, Flag2).


one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag), 
    state(Acc2, Pc2, Mem2, In2, Out2, Flag2)

) :- instr_in_mem(Pc, Mem, Istr),
     extract_pointer(Istr, Pointer),
     Istr = 902,
     output(Acc, Out, Out2),
     Acc2 is Acc,
     Pc2 is Pc+1,
     append([], Mem, Mem2),
     append([], In, In2),
     copy_term(Flag, Flag2).

execution_loop(

    halted_state(Acc, Pc, Mem, In, Out, Flag), 
    Out

).

execution_loop(
    
    state(Acc, Pc, Mem, In, Out, Flag), 
    OutTot

) :- one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState),
     execution_loop(NewState, OutTot).