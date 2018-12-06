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

) :- instr_in_mem(Pc, Mem, Istr),
     Istr >= 0,
     Istr < 100,
     !.

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag),
    halted_state(Acc, Pc, Mem, In, Out, Flag)

) :- instr_in_mem(Pc, Mem, Istr),
     Istr >= 400,
     Istr < 500,
     !.

one_instruction(

    state(Acc, Pc, Mem, In, Out, Flag),
    halted_state(Acc, Pc, Mem, In, Out, Flag)

) :- instr_in_mem(Pc, Mem, Istr),
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

/**
 * Execution Loop
 *
 * Cicla dallo stato iniziale allo stato finale
 *
 * execution_loop(State, Out) / 2
 * 
 * State: rappresenta lo stato iniziale del LMC
 * Out: coda di output nel momento in cui viene raggiunto uno stato di stop
 *
 * MEMO: "Il predicato deve fallire nel caso l’esecuzione termini senza 
 * eseguire una istruzione di halt (ad esempio se si incontra una 
 * istruzione non valida)."
 */
execution_loop(

    halted_state(Acc, Pc, Mem, In, Out, Flag), 
    Out

).

execution_loop(
    
    state(Acc, Pc, Mem, In, Out, Flag), 
    OutTot

) :- one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState),
     execution_loop(NewState, OutTot).



% ISTRUZIONI ASSEMBLY

/**
 * Remove All
 *
 * Rimuove tutte le occorrenze di un elemento da una lista
 *
 * remove(List, Elem, NewList) / 3
 */
remove_all([], _, []).
remove_all([X|T], X, L) :- remove_all(T, X, L).
remove_all([H|T], X, [H|L]) :- H \= X,
                                remove_all(T, X, L).

/**
 * Remove Comment
 * 
 * Rimuove i commenti da un comando
 * Esempio: "add VALUE // comment" -> "add VALUE"
 *
 * remove_comment(Command, CommandWithoutComments) / 2
 */
remove_comment(Row, Command) :- split_string(Row, "//", " ", X),
                                nth0(0, X, Command, _).

/**
 * del_blank
 * 
 * Rimozione stringhe vuote ?
 *
 * del_blank() / 3
 */
del_blank(X, [], []) :- !.
del_blank(X, [X|Xs], Y) :- !, del_blank(X, Xs, Y).
del_blank(X, [T|Xs], Y) :- !, del_blank(X, Xs, Y2), append([T], Y2, Y).

/**
 * Search Label
 * 
 * Ricerca label in una riga
 * e memorizza l'associazione nella base di conoscenza.
 */
search_label(FirstEmptyIndex, Row) :- split_string(Row, " ", "", Y), % splitta istruzione in parole
                              del_blank("", Y, Words), % eliminazione spazi inutili                                           
                              proper_length(Words, WordsNum), % conteggio parole
                              WordsNum = 3,
                              nth0(0, Words, Label, _),
                              assertz(tag(Label, FirstEmptyIndex)).

/**
 * Exec
 *
 * Esecuzione di una riga del programma
 * Legge la riga e restituisce l'istruzione relativa
 * 
 * exec(FirstEmptyIndex, Row, Instruction) / 3
 */
exec(FirstEmptyIndex, Row, Instruction) :- remove_comment(Row, Command),
                                           split_string(Command, " ", "", Y), % splitta istruzione in parole
                                           del_blank("", Y, Words), % eliminazione spazi inutili
                                           proper_length(Words, WordsNum), % conteggio parole WordsNum /= 0,
                                           WordsNum = 0, !.

exec(FirstEmptyIndex, Row, Instruction) :- remove_comment(Row, Command),
                                           split_string(Command, " ", "", Y), % splitta istruzione in parole
                                           del_blank("", Y, Words), % eliminazione spazi inutili
                                           proper_length(Words, WordsNum), % conteggio parole WordsNum /= 0,
                                           WordsNum = 1,
                                           !,
                                           single_command(Words, Instruction).

exec(FirstEmptyIndex, Row, Instruction) :- remove_comment(Row, Command),
                                           split_string(Command, " ", "", Y), % splitta istruzione in parole
                                           del_blank("", Y, Words), % eliminazione spazi inutili
                                           proper_length(Words, WordsNum), % conteggio parole WordsNum /= 0,
                                           WordsNum = 2,
                                           !,
                                           command(Words, Instruction).

exec(FirstEmptyIndex, Row, Instruction) :- remove_comment(Row, Command),
                                           split_string(Command, " ", "", Y), % splitta istruzione in parole
                                           del_blank("", Y, Words), % eliminazione spazi inutili
                                           proper_length(Words, WordsNum), % conteggio parole WordsNum /= 0,
                                           WordsNum = 3,
                                           !,
                                           command_with_label(Words, Instruction, FirstEmptyIndex).

/**
 * Normalize
 * 
 * Numero a 2 cifre
 * Esempio: 4 -> 04, 12 -> 12
 *
 * normalize(Number, NumberNorm) / 2
 */
normalize(Number, NumberNorm) :- string_length(Number, Leng),
                                 Leng = 1,
                                 !,
                                 string_concat("0", Number, NumberNorm).
                                 
normalize(Number, Number).

/**
 * Single Command
 * 
 * Esecuzione di un comando con un comando
 * Restituisce l'istruzione relativa al comando
 *
 * single_command([Command], Instruction) / 2
 */
single_command([Command], Instruction) :- string_lower(Command, CommandLower),
                                          CommandLower = "inp",
                                          !,
                                          copy_term("901", Instruction).

single_command([Command], Instruction) :- string_lower(Command, CommandLower),
                                          CommandLower = "out",
                                          !,
                                          copy_term("902", Instruction).

single_command([Command], Instruction) :- string_lower(Command, CommandLower),
                                          CommandLower = "hlt",
                                          !,
                                          copy_term("001", Instruction).

single_command([Command], Instruction) :- string_lower(Command, CommandLower),
                                          CommandLower = "dat",
                                          !,
                                          copy_term("000", Instruction).

/**
 * Command
 * 
 * Esecuzione di un comando con un comando e un valore (o label)
 * Restituisce l'istruzione relativa al comando
 *
 * command([Command, Value], Instruction) / 2
 */
command([Command, Value], Instruction) :- number_string(_, Value),
                                          !,
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "add",
                                          !,
                                          string_concat("1", ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "add",
                                          !,
                                          string_concat("1", ValueNorm, Instruction).

command([Command, Value], Instruction) :- number_string(_, Value),
                                          !,
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),          
                                          CommandLower = "sub",
                                          !,
                                          string_concat("2", ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),          
                                          CommandLower = "sub",
                                          !,
                                          string_concat("2", ValueNorm, Instruction).

command([Command, Value], Instruction) :- number_string(_, Value),
                                          !, 
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "sta",
                                          !,
                                          string_concat("3", ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "sta",
                                          !,
                                          string_concat("3", ValueNorm, Instruction).

command([Command, Value], Instruction) :- number_string(_, Value),
                                          !,
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "lda",
                                          !,
                                          string_concat("5", ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "lda",
                                          !,
                                          string_concat("5", ValueNorm, Instruction).

command([Command, Value], Instruction) :- number_string(_, Value),
                                          !,
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "bra",
                                          !,
                                          string_concat("6", ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "bra",
                                          !,
                                          string_concat("6", ValueNorm, Instruction).

command([Command, Value], Instruction) :- number_string(_, Value),
                                          !,
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "brz",
                                          !,
                                          string_concat("7", ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "brz",
                                          !,
                                          string_concat("7", ValueNorm, Instruction).

command([Command, Value], Instruction) :- number_string(_, Value),
                                          !,
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "brp",
                                          !,
                                          string_concat("8", ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "brp",
                                          !,
                                          string_concat("8", ValueNorm, Instruction).

command([Command, Value], Instruction) :- number_string(_, Value),
                                          !,
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "dat",
                                          !,
                                          copy_term(ValueNorm, Instruction).

command([Command, Label], Instruction) :- tag(Label, Value),
                                          string_lower(Command, CommandLower), 
                                          normalize(Value, ValueNorm),           
                                          CommandLower = "dat",
                                          !,
                                          copy_term(ValueNorm, Instruction).
/**
 * Command with Label
 * 
 * Esecuzione di un comando con una label, un comando e un valore (o label)
 * Restituisce l'istruzione relativa al comando
 *
 * command_with_label([Label, Command, Value], Instruction, FirstEmptyIndex) / 3
 */
command_with_label([Label, Command, Value], Instruction, FirstEmptyIndex) :- string_lower(Command, CommandLower),  
                                                                             normalize(Value, ValueNorm),
                                                                             assertz(tag(Label, FirstEmptyIndex)),
                                                                             command([Command, Value], Instruction).
                                                                                                                          
/**
 * Row to Mem
 * 
 * Trascrive l'istruzione corrispondente ad un comando in memoria
 *
 * row_to_mem(Rows, Mem, Pc) / 3
 */
row_to_mem([], [], 0) :- !.

row_to_mem([LastRow], Mem, Pc) :- !,
                                  exec(Pc, LastRow, Instruction),
                                  Pc =< 100,
                                  nth0(Pc, Mem, Instruction).

row_to_mem([Row|OtherRows], Mem, Pc) :- exec(Pc, Row, Instruction),
                                        Pc =< 100,
                                        nth0(Pc, Mem, Instruction),
                                        PcNew is Pc+1,
                                        row_to_mem(OtherRows, Mem, PcNew).

/**
 * Save Labels
 * 
 * Salva le label alla prima lettura
 *
 * save_labels(Rows, Mem, Pc) / 3
 */
save_labels([], 0) :- !.

save_labels([LastRow], Pc) :- Pc < 100,
                              remove_comment(LastRow, Command),
                              split_string(Command, " ", "", Words),
                              proper_length(Words, WordsNum),
                              WordsNum < 3,
                              !.

save_labels([LastRow], Pc) :- !,
                              Pc < 100,
                              remove_comment(LastRow, Command),
                              split_string(Command, " ", "", Words),
                              proper_length(Words, WordsNum),
                              WordsNum = 3,
                              !,
                              nth0(0, Words, Label),
                              assertz(tag(Label, Pc)).

save_labels([Row|OtherRows], Pc) :- !,
                                    save_labels([Row], Pc),
                                    PcNew is Pc+1,
                                    save_labels(OtherRows, PcNew).                                                                                            

/**
 * LMC Load
 * 
 * Legge il file assembler
 * Scompatta le righe
 * Legge i comandi sulle righe
 * Inserisce in memoria le istruzioni relative ai comandi
 *
 * lmc_load(Filename, Mem) / 2
 */

noComment([], []).
noComment([H | T], [H2| T2]) :- remove_comment(H, H2),
                                noComment(T, T2).

lmc_load(Filename, Mem) :- open(Filename, read, Input),
                           read_string(Input, _, FileTxt),
                           split_string(FileTxt, "\n", " ", Rows),
                           del_blank("", Rows, ClearRows),
                           %write(ClearRows),
                           /*noComment(ClearRows, NoCommentList),
                           delete(NoCommentList, "", CommandList),
                           write(CommandList),*/
                           save_labels(ClearRows, 0),
                           row_to_mem(ClearRows, Mem, 0),
                           write(Mem).