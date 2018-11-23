% ISTRUZIONI

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
*/

branch(Pc, Pointer) :- Pc is Pointer.

/**
 * Branch if zero
 * 
 * Instruction: 7xx
 *
 * Salto condizionale. 
 * Imposta il valore del program counter a xx solamente se 
 * il contenuto dell'accumulatore è zero e se il flag è assente.
*/

branchifzero(Pc, Acc, Pointer, Flag, NewPc) :- (Acc = 0, Flag = noflag)->
                                        NewPc is Pointer;
                                        NewPc is Pc+1.

/**
 * Branch if positive
 * 
 * Instruction: 8xx
 *
 * Salto condizionale. 
 * Imposta il valore del program counter a xx solamente se il flag è assente.
*/

branchifpositive(Pc, Pointer, Flag, NewPc) :- Flag = noflag -> NewPc is Pointer;
                                        NewPc is Pc+1.

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
 * State
*/

/*state(Acc, Pc, Mem, In, Out, Flag) :-
    numlist(0, 99, L), % lista da 0 a 99
    member(Acc, L), % 0-99 check 
    member(Pc, L), % 0-99 check
    proper_length(Mem, MemLength),
    MemLength < 100, % mem lunga massimo 99
    MemLength >= 0,
    max_member(MaxMem, Mem),
    MaxMem < 1000, % massimo minore di 999
    max_member(MinMem, Mem),
    MinMem >= 0, 
    max_member(MaxIn, In),
    MaxIn< 1000,
    min_member(MinIn, In),
    MinIn >= 0, 
    proper_length(In, InLength),
    InLength >= 0,
    max_member(MaxOut, Out),
    MaxOut< 1000,
    max_member(MinOut, Out),
    MinOut >= 0, 
    proper_length(Out, OutLength),
    OutLength >= 0,
    member(Flag,  [flag, noflag]).
*/
/**
 * One Instruction
*/

istruzione(Pc, Mem , I) :- nth0(Pc, Mem, I, _).

cellarisultato(I, X) :- number_chars(I, L),
                        nth0(0, L, _, L2),
                        number_chars(X, L2).

halted_state(Acc, Pc, Mem, In, Out, Flag).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                  halted_state(Acc, Pc, Mem, In, Out, Flag)) :- 
                  istruzione(Pc, Mem , Istr),
                  ((Istr >= 0, Istr < 100); (Istr >= 400, Istr < 500)),
                  !.

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                  state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    istruzione(Pc, Mem , Istr),
    cellarisultato(Istr, Pointer),
    proper_length(In, InEmpty),
    (
        InEmpty = 0 -> write("input vuoto"),
                       write("\n"),
                       abort;
        Istr >= 100, Istr < 199 -> addizione(Acc, Pointer, Mem, Acc2, Flag2),
                                Pc2 is Pc+1,
                                append([], Mem, Mem2),
                                append([], In, In2),
                                append([], Out, Out2);
        Istr >=200, Istr < 299 -> sottrazione(Acc, Pointer, Mem, Acc2, Flag2),
                                Pc2 is Pc+1,
                                append([], Mem, Mem2),
                                append([], In, In2),
                                append([], Out, Out2);
        Istr >= 300, Istr < 400 -> store(Acc, Pointer, Mem, Mem2),
                                Acc2 is Acc,
                                Pc2 is Pc+1,
                                append([], In, In2),
                                append([], Out, Out2),
                                copy_term(Flag, Flag2);
        Istr >= 500, Istr < 600 -> load(Acc2, Pointer, Mem),                                   
                                   Pc2 is Pc+1,
                                   append([], Mem, Mem2),
                                   append([], In, In2),
                                   append([], Out, Out2),
                                   copy_term(Flag, Flag2);
        Istr >= 600, Istr < 700 -> branch(Pc2, Pointer),
                                   Acc2 is Acc,
                                   append([], Mem, Mem2),
                                   append([], In, In2),
                                   append([], Out, Out2),
                                   copy_term(Flag, Flag2);
        Istr >= 700, Istr < 800 -> branchifzero(Pc, Acc, Pointer, Flag, Pc2),
                                   Acc2 is Acc,
                                   append([], Mem, Mem2),
                                   append([], In, In2),
                                   append([], Out, Out2),
                                   copy_term(Flag, Flag2);
        Istr >= 800, Istr < 900 -> branchifpositive(Pc, Pointer, Flag, Pc2),
                                   Acc2 is Acc,
                                   append([], Mem, Mem2),
                                   append([], In, In2),
                                   append([], Out, Out2),
                                   copy_term(Flag, Flag2);
        Istr =  901 -> input(Acc2, In, In2),
                       Pc2 is Pc+1,
                       append([], Mem, Mem2),
                       append([], Out, Out2),
                       copy_term(Flag, Flag2);
        Istr =  902 -> output(Acc, Out, Out2),
                       Acc2 is Acc,
                       Pc2 is Pc+1,
                       append([], Mem, Mem2),
                       append([], In, In2),
                       copy_term(Flag, Flag2)                 
    ).


%---------------------------------------------
/*state(Acc, Pc, Mem, In, Out, Flag) :-
    numlist(0, 99, L), % lista da 0 a 99
    member(Acc, L), % 0-99 check 
    member(Pc, L), % 0-99 check
    randseq(99, 999, Mem), % 99 elementi random (range: 0-99)
    numlist(0, 999, In), % lista da 0 a 999
    numlist(0, 999, Out), % lista da 0 a 999
    member(Flag,  [flag, noflag]).*/
%---------------------------------------------

/* capisco numero istruzione */
/*istruzione(Pc, Mem , I) :- randseq(99, 999, Mem),
                           numlist(0, 99, L),
                           member(Pc, L),
                           nth0(Pc, Mem, I, _).*/

/* istruzione nxx tengo xx */


execution_loop(halted_state(Acc, Pc, Mem, In, Out, Flag), OutTot).
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), OutTot) :-
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag), state(Acc2, Pc2, Mem2, In2, Out2, Flag2)),
    execution_loop(state(Acc2, Pc2, Mem2, In2, Out2, Flag2), OutTot).


/* parsing */

remove_comment(Row, Command) :- split_string(Row, "//", " ", X),
                                nth0(0, X, Command, _).

del_blank(X, [], []) :- !.
del_blank(X, [X|Xs], Y) :- !, del_blank(X, Xs, Y).
del_blank(X, [T|Xs], Y) :- !, del_blank(X, Xs, Y2), append([T], Y2, Y).

/*exec(Row) :- split_string(Row, " ", "", Y),
             del_blank("", Y, Words),
             proper_length(Words, WordsNum),*/
             

                         