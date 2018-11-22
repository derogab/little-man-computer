% ISTRUZIONI

/**
 * Addizione
 * 
 * Valore: 1xx
*/
%pointer valore già tranciato 

addizione(Acc, Pointer, X, flag) :- randseq(99, 999, Mem), % memoria random
                                    nth0(Pointer, Mem, Value, _),
                                    Y is Acc+Value,
                                    X is ((Acc+Value)mod 1000),
                                    Y > 999.
addizione(Acc, Pointer, X, noflag) :- randseq(99, 999, Mem), % memoria random
                                    nth0(Pointer, Mem, Value, _),
                                    Y is Acc+Value,
                                    X is ((Acc+Value)mod 1000),
                                    Y < 1000.

/**
 * Sottrazione
 * 
 * Value: 2xx
*/

sottrazione(Acc, Pointer, X, flag) :- randseq(99, 999, Mem), % memoria random
                                      nth0(Pointer, Mem, Value, _),
                                      Y is Acc-Value,
                                      X is ((Acc-Value)mod 1000),
                                      Y < 0.
sottrazione(Acc, Pointer, X, noflag) :- randseq(99, 999, Mem), % memoria random
                                        nth0(Pointer, Mem, Value, _),
                                        Y is Acc-Value,
                                        X is ((Acc-Value)mod 1000),
                                        Y > -1.

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
                                        branch(Pc, Pointer).

/**
 * Branch if positive
 * 
 * Value: 8xx
*/

branchifpositive(Pc, Pointer, Flag) :- Flag = noflag,
                                       branch(Pc, Pointer).

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
    MemLength < 99, % mem lunga massimo 99
    max_member(Max, Mem),
    Max < 999, % massimo minore di 999
    numlist(0, 999, In), % lista da 0 a 999
    numlist(0, 999, Out), % lista da 0 a 999
    member(Flag,  [flag, noflag]).

/**
 * One Instruction
*/
one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag),
    state(Acc2, Pc2, Mem2, In2, Out2, FLag2)

) :- istruzione(Pc, Mem , Istr),
     addizione(Acc, Istr, Acc2).
     sottrazione(Acc, Istr, Acc2),
     store(Acc, Istr, Mem2),
     load(Acc, Istr, Mem2),
     branch(Pc, Istr) ,
     branchifzero(Pc, Acc, Istr) ,
     branchifpositive(Pc, Istr, Flag) ,
     input(Acc, In, Istr, NewIn) ,
     output(Acc, Out, Istr, NewOut).



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

