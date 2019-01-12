%%%% Little Man Computer
%%%% Progetto di Linguaggi di Programmazione
%%%% Anno accademico 2018-2019
%%%% Appello di Gennaio 2019

%%% Addizione
%%% Instruction: 1xx 
addizione(Acc, Pointer, Mem, X, flag) :- 
    nth0(Pointer, Mem, Value, _),
    Y is Acc + Value,
    X is ((Acc + Value) mod 1000),
    Y > 999,
    !.

addizione(Acc, Pointer, Mem, X, noflag) :- 
    nth0(Pointer, Mem, Value, _),
    Y is Acc+Value,
    X is ((Acc+Value) mod 1000),
    Y < 1000.

%%% Sottrazione
%%% Instruction: 2xx
sottrazione(Acc, Pointer, Mem, X, flag) :- 
    nth0(Pointer, Mem, Value, _),
    Y is Acc-Value,
    X is ((Acc-Value)mod 1000),
    Y < 0,
    !.

sottrazione(Acc, Pointer, Mem, X, noflag) :- 
    nth0(Pointer, Mem, Value, _),
    Y is Acc-Value,
    X is ((Acc-Value)mod 1000),
    Y >= 0.

%%% Store
%%% Instruction: 3xx
store(Acc, Pointer, MemIn, MemOut) :- 
    nth0(Pointer, MemIn, _, Varmem),
    nth0(Pointer, MemOut, Acc, Varmem).

%%% Load
%%% Instruction: 5xx
load(Acc, Pointer, MemIn) :- 
    nth0(Pointer, MemIn, Acc, _).

%%% Branch
%%% Instruction: 6xx
branch(Pc, Pc).

%%% Branch if zero
%%% Instruction: 7xx
branchifzero(_, 0, Pointer, noflag, Pointer) :- 
    !.

branchifzero(Pc, _, _, noflag, NewPc) :- 
    NewPc is ((Pc + 1) mod 100).

branchifzero(Pc, _, _, flag, NewPc) :- 
    NewPc is ((Pc + 1) mod 100).

%%% Branch if positive
%%% Instruction: 8xx
branchifpositive(_, Pointer, noflag, Pointer) :- 
    !.

branchifpositive(Pc, _, flag, NewPc) :- 
    NewPc is ((Pc + 1) mod 100).

%%% Input
%%% Instruction: 901
input(Acc, [Acc|NewQueueIn], NewQueueIn).

%%% Output
%%% Instruction: 902
output(Acc, QueueOut, NewQueueOut) :- 
    append(QueueOut, [Acc], NewQueueOut).

%%% Little Man Computer

%%% Istruzione in memoria
instr_in_mem(Pc, Mem, Instruction) :- 
    nth0(Pc, Mem, Instruction, _).

%%% Puntatore nell'istruzione
extract_pointer(I, X) :- 
    X is (I mod 100).

%%% One Instruction
%%% Passaggio da uno stato iniziale ad uno stato finale
%%% a seconda del valore dell'opcode (Istr)
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), 
                halted_state(Acc, Pc, Mem, In, Out, Flag)) :- 
    instr_in_mem(Pc, Mem, Istr),
    Istr >= 0,
    Istr < 100,
    !.

one_instruction(state(Acc, Pc, Mem, In, Out, _), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    extract_pointer(Istr, Pointer),
    Istr >= 100,
    Istr < 200,
    addizione(Acc, Pointer, Mem, Acc2, Flag2),
    Pc2 is ((Pc + 1) mod 100),
    append([], Mem, Mem2),
    append([], In, In2),
    append([], Out, Out2).

one_instruction(state(Acc, Pc, Mem, In, Out, _), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    extract_pointer(Istr, Pointer),
    Istr >= 200,
    Istr < 300,
    sottrazione(Acc, Pointer, Mem, Acc2, Flag2),
    Pc2 is ((Pc + 1) mod 100),
    append([], Mem, Mem2),
    append([], In, In2),
    append([], Out, Out2).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    extract_pointer(Istr, Pointer),
    Istr >= 300,
    Istr < 400,
    store(Acc, Pointer, Mem, Mem2),
    Acc2 is Acc,
    Pc2 is ((Pc + 1) mod 100),
    append([], In, In2),
    append([], Out, Out2),
    copy_term(Flag, Flag2).

one_instruction(state(_, Pc, Mem, In, Out, Flag), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    extract_pointer(Istr, Pointer),
    Istr >= 500,
    Istr < 600,
    load(Acc2, Pointer, Mem),                                   
    Pc2 is ((Pc + 1) mod 100),
    append([], Mem, Mem2),
    append([], In, In2),
    append([], Out, Out2),
    copy_term(Flag, Flag2).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    extract_pointer(Istr, Pointer),
    Istr >= 600,
    Istr < 700,
    branch(Pc2, Pointer),
    Acc2 is Acc,
    append([], Mem, Mem2),
    append([], In, In2),
    append([], Out, Out2),
    copy_term(Flag, Flag2).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    extract_pointer(Istr, Pointer),
    Istr >= 700,
    Istr < 800,
    branchifzero(Pc, Acc, Pointer, Flag, Pc2),
    Acc2 is Acc,
    append([], Mem, Mem2),
    append([], In, In2),
    append([], Out, Out2),
    copy_term(Flag, Flag2).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    extract_pointer(Istr, Pointer),
    Istr >= 800,
    Istr < 900,
    branchifpositive(Pc, Pointer, Flag, Pc2),
    Acc2 is Acc,
    append([], Mem, Mem2),
    append([], In, In2),
    append([], Out, Out2),
    copy_term(Flag, Flag2).

one_instruction(state(_, Pc, Mem, In, Out, Flag), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    Istr = 901,
    proper_length(In, InEmpty),
    InEmpty \= 0,
    input(Acc2, In, In2),
    Pc2 is ((Pc + 1) mod 100),
    append([], Mem, Mem2),
    append([], Out, Out2),
    copy_term(Flag, Flag2).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), 
                state(Acc2, Pc2, Mem2, In2, Out2, Flag2)) :- 
    instr_in_mem(Pc, Mem, Istr),
    Istr = 902,
    output(Acc, Out, Out2),
    Acc2 is Acc,
    Pc2 is ((Pc + 1) mod 100),
    append([], Mem, Mem2),
    append([], In, In2),
    copy_term(Flag, Flag2).

%%% Check List
%%% Controlla che una lista non abbia valori superiori a 999
%%% E che abbia solo valori positivi
checklist([]).

checklist([H|T]) :- 
    H =< 999,
    H >= 0,
    !,
    checklist(T).

%%% Execution Loop
%%% Cicla dallo stato iniziale allo stato finale
%%% Restituisce la coda di output quando viene raggiunto uno stato di halt
execution_loop(halted_state(_, _, _, _, Out, _), Out).

execution_loop(state(Acc, Pc, Mem, In, Out, Flag), OutTot) :-
    integer(Pc),
    Pc < 100,
    integer(Acc),
    Acc < 1000,
    proper_length(Mem, Len),
    Len =< 100,
    member(Flag, [flag, noflag]),
    checklist(Mem),
    checklist(In),
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState),
    !,
    execution_loop(NewState, OutTot).

%%% Remove Comment
%%% Rimuove i commenti da una stringa 
remove_comment(Row, Command) :- %% pulizia labels
    split_string(Row, "//", " ", X),
    nth0(0, X, Command, _).

%%% del_blank
%%% Elimina tutti gli elementi uguali alla stringa vuota da una lista
del_blank(_, [], []) :- 
    !.

del_blank(X, [X|Xs], Y) :- 
    !, 
    del_blank(X, Xs, Y).

del_blank(X, [T|Xs], Y) :- 
    !, 
    del_blank(X, Xs, Y2), 
    append([T], Y2, Y).

%%% noInstr
%%% Controlla che un elemento non sia presente in una lista
noInstr(_,[]) :- 
    !.

noInstr(X,[X|_]) :-
    !,
    fail.

noInstr(X,[_|T]) :- 
    !,
    noInstr(X,T).

%%% Exec
%%% Esecuzione di un comando assemply
%%% differenziato a seconda del numero di parole contenute
%%% e dalla eventuale presenza di label 
exec(_, Row, _) :- 
    remove_comment(Row, Command),
    split_string(Command, " ", "", Y),
    del_blank("", Y, Words),
    proper_length(Words, WordsNum),
    WordsNum = 0, !.

exec(_, Row, Instruction) :- 
    remove_comment(Row, Command),
    split_string(Command, " ", "", Y), 
    del_blank("", Y, Words), 
    proper_length(Words, WordsNum), 
    WordsNum = 1,
    !,
    single_command(Words, Instruction).

exec(FirstEmptyIndex, Row, Instruction) :- 
    remove_comment(Row, Command),
    split_string(Command, " ", "", Y), 
    del_blank("", Y, Words), 
    proper_length(Words, WordsNum),
    WordsNum = 2,
    nth0(0, Words, Elem),
    string_lower(Elem, Eleml),
    noInstr(Eleml, ["add", "sub", "sta", "lda", "bra", "brz"]), 
    noInstr(Eleml, ["brp", "inp", "out", "hlt", "dat"]),
    !,
    command_with_label2(Words, Instruction, FirstEmptyIndex).

exec(_, Row, Instruction) :- 
    remove_comment(Row, Command),
    split_string(Command, " ", "", Y), 
    del_blank("", Y, Words), 
    proper_length(Words, WordsNum),
    WordsNum = 2,
    !,
    command(Words, Instruction).

exec(FirstEmptyIndex, Row, Instruction) :- 
    remove_comment(Row, Command),
    split_string(Command, " ", "", Y), 
    del_blank("", Y, Words), 
    proper_length(Words, WordsNum), 
    WordsNum = 3,
    !,
    command_with_label(Words, Instruction, FirstEmptyIndex).

%%% Normalize
%%% Restituisce qualunque numero in 2 cifre
%%% aggiungendo uno 0 prima nel caso sia un numero compreso tra 0 e 10
normalize(Number, NumberNorm) :- 
    string_length(Number, Leng),
    Leng = 1,
    !,
    string_concat("0", Number, NumberNorm).
                                 
normalize(Number, Number).

%%% Single Command
%%% Restituisce l'istruzione numerica associata al comando assembly
%%% nel caso di un comando con una singola parola
single_command([Command], Instruction) :- 
    string_lower(Command, CommandLower),
    CommandLower = "inp",
    !,
    copy_term("901", Instruction).

single_command([Command], Instruction) :- 
    string_lower(Command, CommandLower),
    CommandLower = "out",
    !,
    copy_term("902", Instruction).

single_command([Command], Instruction) :- 
    string_lower(Command, CommandLower),
    CommandLower = "hlt",
    !,
    copy_term("000", Instruction).

single_command([Command], Instruction) :- 
    string_lower(Command, CommandLower),
    CommandLower = "dat",
    !,
    copy_term("000", Instruction).

%%% Command 
%%% Restituisce l'istruzione numerica associata al comando assembly
%%% nel caso di un comando con 2 parole 
%%% (label + singolo comando oppure comando + valore)
command([Command, Value], Instruction) :- 
    number_string(_, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "add",
    !,
    string_concat("1", ValueNorm, Instruction).

command([Command, Label], Instruction) :- 
    string_upper(Label, LabelUpper),
    tag(LabelUpper, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "add",
    !,
    string_concat("1", ValueNorm, Instruction).

command([Command, Value], Instruction) :- 
    number_string(_, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),          
    CommandLower = "sub",
    !,
    string_concat("2", ValueNorm, Instruction).

command([Command, Label], Instruction) :- 
    string_upper(Label, LabelUpper),
    tag(LabelUpper, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),          
    CommandLower = "sub",
    !,
    string_concat("2", ValueNorm, Instruction).

command([Command, Value], Instruction) :- 
    number_string(_, Value),            
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "sta",
    !,
    string_concat("3", ValueNorm, Instruction).

command([Command, Label], Instruction) :- 
    string_upper(Label, LabelUpper),
    tag(LabelUpper, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "sta",
    !,
    string_concat("3", ValueNorm, Instruction).

command([Command, Value], Instruction) :- 
    number_string(_, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "lda",
    !,
    string_concat("5", ValueNorm, Instruction).

command([Command, Label], Instruction) :- 
    string_upper(Label, LabelUpper),
    tag(LabelUpper, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "lda",
    !,
    string_concat("5", ValueNorm, Instruction).

command([Command, Value], Instruction) :- 
    number_string(_, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "bra",
    !,
    string_concat("6", ValueNorm, Instruction).

command([Command, Label], Instruction) :- 
    string_upper(Label, LabelUpper),
    tag(LabelUpper, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "bra",
    !,
    string_concat("6", ValueNorm, Instruction).

command([Command, Value], Instruction) :- 
    number_string(_, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "brz",
    !,
    string_concat("7", ValueNorm, Instruction).

command([Command, Label], Instruction) :- 
    string_upper(Label, LabelUpper),
    tag(LabelUpper, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "brz",
    !,
    string_concat("7", ValueNorm, Instruction).

command([Command, Value], Instruction) :- 
    number_string(_, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "brp",
    !,
    string_concat("8", ValueNorm, Instruction).

command([Command, Label], Instruction) :- 
    string_upper(Label, LabelUpper),
    tag(LabelUpper, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "brp",
    !,
    string_concat("8", ValueNorm, Instruction).

command([Command, Value], Instruction) :- 
    number_string(_, Value),
    string_lower(Command, CommandLower), 
    normalize(Value, ValueNorm),           
    CommandLower = "dat",
    !,
    copy_term(ValueNorm, Instruction).

%%% Command with Label
%%% Restituisce l'istruzione numerica associata al comando assembly
%%% nel caso di un comando con 3 parole
%%% (label + comando + valore)
command_with_label([_, Command, Value], Instruction, _) :- 
    string_lower(Command, CommandLower),  
    command([CommandLower, Value], Instruction).

command_with_label2([_, Command], Instruction, _) :- 
    string_lower(Command, CommandLower),  
    single_command([CommandLower], Instruction).       

%%% Replace
%%% Sostituisce l'elemento X nella lista L in posizione I
%%% nel parametro L2
replace(X, L, I, L2) :- 
    nth0(X, L, _, L3), 
    nth0(X, L2, I, L3).

%%% Row to Mem
%%% Richiama ricorsivamente exec riempiendo la memoria
row_to_mem([], [], 0, []) :- 
    !.

row_to_mem([LastRow], Mem, Pc, MemOut) :- 
    !,
    exec(Pc, LastRow, Instruction),
    Pc =< 100,
    replace(Pc, Mem, Instruction, MemOut).

row_to_mem([Row|OtherRows], Mem, Pc, MemOut) :- 
    exec(Pc, Row, Instruction),
    Pc =< 100,
    replace(Pc, Mem, Instruction, MemOutNew),
    PcNew is Pc+1,
    row_to_mem(OtherRows, MemOutNew, PcNew, MemOut).

%%% Save Labels
%%% Salva tutte le label presenti nel file .lmc
%%% aggiungendoli con gli assert alla base di conoscenza 
save_labels([], 0) :- 
    !.

save_labels([LastRow], Pc) :- 
    Pc < 100,
    remove_comment(LastRow, Command),
    split_string(Command, " ", " ", Words),
    proper_length(Words, WordsNum),
    WordsNum < 2,
    !.

save_labels([LastRow], Pc) :- 
    Pc < 100,
    remove_comment(LastRow, Command),
    split_string(Command, " ", " ", Words),
    proper_length(Words, WordsNum),
    WordsNum = 2,
    nth0(0, Words, Label),
    noInstr(Label, ["add", "sub", "sta", "lda", "bra", "brz"]),
    noInstr(Label, ["brp", "inp", "out", "hlt", "dat"]),
    !, 
    string_upper(Label, LabelUpper),
    assertz(tag(LabelUpper, Pc)).

save_labels([LastRow], Pc) :- 
    Pc < 100,
    remove_comment(LastRow, Command),
    split_string(Command, " ", " ", Words),
    proper_length(Words, WordsNum),
    WordsNum = 2,
    !.

save_labels([LastRow], Pc) :- 
    Pc < 100,
    remove_comment(LastRow, Command),
    split_string(Command, " ", " ", Words),
    proper_length(Words, WordsNum),
    WordsNum = 3,
    !,
    nth0(0, Words, Label),
    string_upper(Label, LabelUpper),
    assertz(tag(LabelUpper, Pc)).

save_labels([Row|OtherRows], Pc) :-
    !,
    save_labels([Row], Pc),
    PcNew is Pc+1,
    save_labels(OtherRows, PcNew).                                                                                            

%%% noComment
%%% Rimuove ogni riga di commento dalla lista di stringhe 
noComment([], []).

noComment([H | T], [H2| T2]) :- 
    remove_comment(H, H2),
    noComment(T, T2).

%%% memg
%%% Riempie il fondo della Mem con gli 0 
memg(L, NewMem) :- 
    proper_length(L, X),
    X<100,
    append(["0"], L, Mem),
    memg(Mem, NewMem), 
    !.

memg(Mem, Mem) :-
    !.

%%% memToNumber
%%% Converte la lista di stringhe in lista di interi
memToNumber(Mem, MemNumber, X) :- 
    X<100,
    nth0(X, Mem, Elem),
    number_string(Num, Elem),
    NewX is X+1,
    replace(X, Mem, Num, MemNumber2),
    memToNumber(MemNumber2, MemNumber, NewX),!.

memToNumber(Mem, Mem, _) :- 
    !.

%%% lmc_load
%%% Legge il file .lmc e genera la memoria
%%% dopo gli opportuni controlli 
%%% (rimozione commenti, salvataggio e sostituzione label)
lmc_load(Filename, Mem) :- 
    open(Filename, read, Input),
    read_string(Input, _, FileTxt),
    split_string(FileTxt, "\n", " ", Rows),
    del_blank("", Rows, ClearRows),
    noComment(ClearRows, NoCommentList),
    delete(NoCommentList, "", CommandList),
    proper_length(CommandList, NumberCommand),
    NumberCommand =< 100,
    save_labels(CommandList, 0),
    memg([], MemV),
    row_to_mem(CommandList, MemV, 0, Mem).

%%% lmc_run
%%% Esegue il lmc_load del file .lmc,
%%% converte gli elementi della memoria in numeri interi
%%% e la passa come memoria dello stato iniziale dell'execution_loop.
%%% Infine rimuove tutte le label aggiunte alla base di conoscenza
lmc_run(Filename, In, Output) :- 
    %% Generazione della memoria
    lmc_load(Filename, Mem),
    %% Conversione memoria in interi
    memToNumber(Mem, MemNumber, 0),
    %% Pulizia delle labels
    retractall(tag(_,_)), 
    %% Esecuzione
    execution_loop(state(0, 0, MemNumber, In, [], noflag), Output),
    !.
