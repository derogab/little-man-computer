% ISTRUZIONI

/**
 * Addizione
 * 
 * Valore: 1xx
*/
%pointer valore già tranciato 

addizione(Acc, Pointer, Mem, X, flag) :- nth0(Pointer, Mem, Value, _),
                                    Y is Acc+Value,
                                    X is ((Acc+Value)mod 1000),
                                    Y > 999,!.
addizione(Acc, Pointer, Mem, X, noflag) :- nth0(Pointer, Mem, Value, _),
                                      Y is Acc+Value,
                                      X is ((Acc+Value)mod 1000),
                                      Y < 1000.


/**
 * Sottrazione
 * 
 * Value: 2xx
*/

sottrazione(Acc, Pointer, Mem, X, flag) :- nth0(Pointer, Mem, Value, _),
                                      Y is Acc-Value,
                                      X is ((Acc-Value)mod 1000),
                                      Y < 0,!.
sottrazione(Acc, Pointer, Mem, X, noflag) :- nth0(Pointer, Mem, Value, _),
                                        Y is Acc-Value,
                                        X is ((Acc-Value)mod 1000),
                                        Y >= 0.

/**
 * Store
 * 
 * Value: 3xx
*/

store(Acc, Pointer, MemIn, MemOut) :- nth0(Pointer, MemIn, _, Varmem),
                                      nth0(Pointer, MemOut, Acc, Varmem).

/**
 * Instruction not found
 * 
 * Value: 4xx
*/
notfound(Pointer) :- write("L'istruzione "), write(Pointer), write(" non è presente!").

/**
 * Load
 * 
 * Value: 5xx
*/
load(Acc, Pointer, MemIn) :- nth0(Pointer, MemIn, Acc, _).

/**
 * Branch
 * 
 * Value: 6xx
*/

branch(Pc, Pointer) :- Pc is Pointer.

/**
 * Branch if zero
 * 
 * Value: 7xx
*/

branchifzero(Pc, Acc, Pointer, Flag) :- Acc = 0,
                                        Flag = noflag,
                                        Pc is Pointer.

/**
 * Branch if positive
 * 
 * Value: 8xx
*/

branchifpositive(Pc, Pointer, Flag) :- Flag = noflag,
                                       Pc is Pointer.

/**
 * Input
 * 
 * Value: 901
*/
input(Acc, [Acc|NewQueueIn], NewQueueIn).

/**
 * Output
 * 
 * Value: 902
*/

output(Acc, QueueOut, NewQueueOut) :- append(QueueOut, [Acc], NewQueueOut).

/**
 * Halt
 * 
 * Value: 0xx
*/

lmc_halt :- halt.



% LITTLE MAN COMPUTER

/**
 * State
*/

state(Acc, Pc, Mem, In, Out, Flag) :-
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

/**
 * One Instruction
*/

istruzione(Pc, Mem , I) :- nth0(Pc, Mem, I, _).

                           
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                  state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    istruzione(Pc, Mem , Istr),
    cellarisultato(Istr, Pointer),
    (
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
        Istr >= 400, Istr < 500 -> notfound(Pointer);
        Istr >= 500, Istr < 600 -> branch(Pc2, Pointer),
                                   Acc2 is Acc,
                                   append([], Mem, Mem2),
                                   append([], In, In2),
                                   append([], Out, Out2),
                                   copy_term(Flag, Flag2);
        Istr >= 600, Istr < 700 -> branchifzero(Pc2, Acc, Pointer, Flag),
                                   Acc2 is Acc,
                                   append([], Mem, Mem2),
                                   append([], In, In2),
                                   append([], Out, Out2);
        Istr >= 700, Istr < 800 -> branchifpositive(Pc2, Pointer, Flag),
                                   Acc2 is Acc,
                                   append([], Mem, Mem2),
                                   append([], In, In2),
                                   append([], Out, Out2)                          
                                  
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
cellarisultato(I, X) :- number_chars(I, L),
                        nth0(0, L, _, L2),
                        number_chars(X, L2).



