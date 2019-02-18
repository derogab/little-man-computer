# Little Man Computer

### PROGETTO DEL CORSO DI LINGUAGGI DI PROGRAMMAZIONE

## OVERVIEW DEL PROGETTO

Il _little man computer_ (LMC) è un semplice modello di computer creato per scopi didattici. Esso possiede 100 celle di memoria (numerate da 0 a 99) ognuna della quali può contenere un numero da 0 a 999 (estremi inclusi). Il computer possiede un solo registro, detto _accumulatore_,
una coda di input ed una coda di output. LMC possiede un numero limitato di tipi di istruzioni ed un equivalente assembly altrettanto semplificato. Lo scopo del progetto è quello di produrre 

- Un simulatore del LMC che dato il contenuto iniziale delle memoria (una lista di 100 numeri) e una sequenza di valori di input simuli il comportamento del LMC e produca il contenuto della coda di output dopo l'arresto del LMC.
- Un assembler che, dato un file scritto nell'assembly semplificato del LMC produca il contenuto iniziale della memoria.

La parte rimanente di questo testo dettaglierà l'architettura del LMC, la sintassi dell'assembly e i
predicati/funzioni che le implementazioni in Prolog e Common Lisp devono implementare.

## ARCHITETTURA DEL LMC

Il LMC è composto dalle seguenti componenti:

- Una **memoria** di 100 celle numerate tra 0 e 99. Ogni cella può contenere un numero tra 0 e 999. Non viene effettuata alcuna distinzione tra dati e istruzioni: a seconda del momento il contenuto di una certa cella può essere interpretato come un'istruzione (con una semantica che verrà spiegata in seguito) oppure come un dato (e, ad esempio, essere sommato ad un altro valore).

- Un **registro accumulatore** (inizialmente zero).

- Un **program counter**. Il program counter contiene l'indirizzo dell'istruzione da eseguire ed è inizialmente zero. Se _non_ viene sovrascritto da altre istruzioni (salti condizionali e non condizionali) viene incrementato di uno ogni volta che un'istruzione viene eseguita. Se
raggiunge il valore 99 e viene incrementato il valore successivo è zero.

- Una **coda di input**. Questa coda contiene tutti i valori forniti in input al LMC, che sono necessariamente numeri compresi tra 0 e 999. Ogni volta che l'LMC legge un valore di input esso viene eliminato dalla coda.

- Una **coda di output**. Questa coda è inizialmente vuota e contiene tutti i valori mandati in output dal LMC, che sono necessariamente numeri compresi tra 0 e 999. La coda è
strutturata in modo tale da avere in testa il primo output prodotto e come ultimo elemento l'ultimo output prodotto: i valori di output sono quindi in ordine cronologico.

- Un **flag**. Ovvero un singolo bit che può essere acceso o spento. Inizialmente esso è zero (flag assente). Solo le istruzioni aritmetiche modificano il valore del flag e, in particolare, un flag a uno (flag presente) indica che l'ultima operazione aritmetica eseguita ha prodotto un
risultato maggiore di 999 o minore di 0. Un flag assente indica invece che il valore prodotto dall'ultima operazione aritmetica ha prodotto un risultato compreso tra 0 e 999.


La seguente tabella rappresenta come le istruzioni presenti in memoria debbano essere interpretate:

Alcuni devi valori non corrispondono a nessuna istruzione. Ad esempio tutti i numeri tra 400 e 499 non hanno un corrispettivo. Questo significa che corrispondono a delle istruzioni non valide (illegal instructions) e che LMC si fermerà con una condizione di errore.

Quindi, ad esempio, se l'accumulatore contiene il valore 42, il program counter ha valore 10 ed il contenuto della cella numero 10 è 307, il LMC eseguirà una istruzione di **store**, dato che il valore è nella forma 3xx. In particolare, il contenuto dell'accumulatore, ovvero 42, verrà scritto nella cella di memoria numero 7 (dato che xx corrisponde a 07). Il program counter verrà poi incrementato di uno, assumendo il valore 11. La procedura verrà ripetuta finché non verrà incontrata un'istruzione di halt o un'istruzione non valida.

<table>
    <tr>
        <td><b>Valore</b></td>
        <td><b>Nome Istruzione</b></td>
        <td><b>Significato</b></td>
    </tr>
    <tr>
        <td>1xx</td>
        <td>Addizione</td>
        <td>Somma il contenuto della cella di memoria xx con il valore contenuto nell'accumulatore e scrive il valore risultante nell'accumulatore. Il valore salvato nell'accumulatore è la somma modulo 1000. Se la somma non supera 1000 il flag è impostato ad assente, se invece raggiunge o supera 1000 il flag è impostato a presente.</td>
    </tr>
    <tr>
        <td>2xx</td>
        <td>Sottrazione</td>
        <td>Sottrae il contenuto della cella di memoria xx dal valore contenuto nell'accumulatore e scrive il valore risultante nell'accumulatore. Il valore salvato nell'accumulatore è la differenza modulo 1000. Se la differenza è inferiore a zero il flag è impostato a presente, se invece è positiva o zero il flag è impostato ad assente.</td>
    </tr>
    <tr>
        <td>3xx</td>
        <td>Store</td>
        <td>Salva il valore contenuto nell'accumulatore nella cella di memoria avente indirizzo xx. Il contenuto dell'accumulatore rimane invariato.</td>
    </tr>
    <tr>
        <td>5xx</td>
        <td>Load</td>
        <td>Scrive il valore contenuto nella cella di memoria di indirizzo xx nell'accumulatore. Il contenuto della cella di memoria rimane invariato.</td>
    </tr>
    <tr>
        <td>6xx</td>
        <td>Branch</td>
        <td>Salto non condizionale. Imposta il valore del program counter a xx.</td>
    </tr>
    <tr>
        <td>7xx</td>
        <td>Branch if zero</td>
        <td>Salto condizionale. Imposta il valore del program counter a xx solamente se il contenuto dell'accumulatore è zero e se il flag è assente.</td>
    </tr>
    <tr>
        <td>8xx</td>
        <td>Branch if positive</td>
        <td>Salto condizionale. Imposta il valore del program counter a xx solamente se il flag è assente.</td>
    </tr>
    <tr>
        <td>901</td>
        <td>Input</td>
        <td>Scrive il contenuto presente nella testa della coda in input nell'accumulatore e lo rimuove dalla coda di input.</td>
    </tr>
    <tr>
        <td>902</td>
        <td>Output</td>
        <td>Scrive il contenuto dell'accumulatore alla fine della coda di output. Il contenuto dell'accumulatore rimane invariato.</td>
    </tr>
    <tr>
        <td>0xx</td>
        <td>Halt</td>
        <td>Termina l'esecuzione del programma. Nessuna ulteriore istruzione viene eseguita.</td>
    </tr>
</table>

## ASSEMBLY PER LMC

Oltre a fornire una simulazione per il LMC si deve essere anche in grado di passare da un programma scritto in codice assembly per LMC (che verrà successivamente dettagliato) al
contenuto iniziale della memoria del LMC, convertendo quindi il codice assembly in codice macchina.

Nel file sorgente assembly ogni riga contiene al più una etichetta ed una istruzione. Ogni istruzione corrisponde al contenuto di una cella di memoria. La prima istruzione assembly presente nel file corrisponderà al valore contenuto nella cella 0, la seconda al valore contenuto
nella cella 1, e così via.

<table>
    <tr>
        <td><b>Istruzione</b></td>
        <td><b>Valori possibili per xx</b></td>
        <td><b>Significato</b></td>
    </tr>
    <tr>
        <td>ADD</td>
        <td>xx</td>
        <td>Indirizzo o etichetta Esegui l'istruzione di addizione tra l'accumulatore e il valore contenuto nella cella indicata da xx</td>
    </tr>
    <tr>
        <td>SUB</td>
        <td>xx</td>
        <td>Indirizzo o etichetta Esegui l'istruzione di sottrazione tra l'accumulatore e il valore contenuto nella cella indicata da xx</td>
    </tr>
    <tr>
        <td>STA</td>
        <td>xx</td>
        <td>Indirizzo o etichetta Esegue una istruzione di store dell'accumulatore nella cella indicata da xx</td>
    </tr>
    <tr>
        <td>LDA</td>
        <td>xx</td>
        <td>Indirizzo o etichetta Esegue una istruzione di load dal valore contenuto nella cella indicata da xx nell'accumulatore</td>
    </tr>
    <tr>
        <td>BRA</td>
        <td>xx</td>
        <td>Indirizzo o etichetta Esegue una istruzione di branch non condizionale al valore indicato da xx</td>
    </tr>
    <tr>
        <td>BRZ</td>
        <td>xx</td>
        <td>Indirizzo o etichetta Esegue una istruzione di branch condizionale (se l'accumulatore è zero e non vi è il flag acceso) al valore indicato da xx.</td>
    </tr>
    <tr>
        <td>BRP</td>
        <td>xx</td>
        <td>Indirizzo o etichetta Esegue una istruzione di branch condizionale (se non vi è il flag acceso) al valore indicato da xx.</td>
    </tr>
    <tr>
        <td>INP</td>
        <td>Nessuno</td>
        <td>Esegue una istruzione di input</td>
    </tr>
    <tr>
        <td>OUT</td>
        <td>Nessuno</td>
        <td>Esegue una istruzione di output</td>
    </tr>
    <tr>
        <td>HLT</td>
        <td>Nessuno</td>
        <td>Esegue una istruzione di halt</td>
    </tr>
    <tr>
        <td>DAT</td>
        <td>xx</td>
        <td>Numero Memorizza nella cella di memoria corrispondente a questa istruzione assembly il valore xx</td>
    </tr>
    <tr>
        <td>DAT</td>
        <td>Nessuno</td>
        <td>Memorizza nella cella di memoria corrispondente a questa istruzione assembly il valore 0 equivalente all'istruzione DAT 0).</td>
    </tr>
</table>

Ogni riga del file assembly con una istruzione può contenere prima dell'istruzione una etichetta, ovvero una stringa di caratteri usata per identificare la cella di memoria dove verrà salvata l'istruzione corrente. Le etichette possono poi essere utilizzate al posto degli indirizzi (ovvero numeri tra 0 e 99) all'interno del sorgente. Si veda, ad esempio:

<table>
    <tr>
        <td><b>Assembly</b></td>
        <td><b>Cella di memoria</b></td>
        <td><b>Codice macchina</b></td>
    </tr>
    <tr>
        <td>BRA LABEL</td>
        <td>1</td>
        <td>600</td>
    </tr>
</table>

In questo caso l'etichetta presente nella prima riga assume il valore 0 tutte le volte che appare come argomento di un'altra istruzione. Per un esempio già completo si osservi:

<table>
    <tr>
        <td><b>Assembly</b></td>
        <td><b>Cella di memoria</b></td>
        <td><b>Codice macchina</b></td>
    </tr>
    <tr>
        <td>LOAD LDA 0</td>
        <td>0</td>
        <td>500</td>
    </tr>
    <tr>
        <td>OUT</td>
        <td>1</td>
        <td>902</td>
    </tr>
    <tr>
        <td>SUB ONE</td>
        <td>2</td>
        <td>208</td>
    </tr>
    <tr>
        <td>BRZ ONE</td>
        <td>3</td>
        <td>708</td>
    </tr>
    <tr>
        <td>LDA LOAD</td>
        <td>4</td>
        <td>500</td>
    </tr>
    <tr>
        <td>ADD ONE</td>
        <td>5</td>
        <td>108</td>
    </tr>
    <tr>
        <td>STA LOAD</td>
        <td>6</td>
        <td>300</td>
    </tr>
    <tr>
        <td>BRA LOAD</td>
        <td>7</td>
        <td>600</td>
    </tr>
    <tr>
        <td>ONE DAT 1</td>
        <td>8</td>
        <td>1</td>
    </tr>
</table>

In questo caso ci sono due etichette (LOAD e ONE) che assumono valore 0 e 8 rispettivamente.
Questo corrisponde alla cella di memoria dove la versione convertita in codice macchina dell'istruzione assembly presente nella stessa riga è stata salvata. Si noti inoltre come le etichette possano essere utilizzate prima di venire definite. Se una riga contiene una doppia barra (`//`) tutto il resto della riga deve venire ignorato e considerato come commento. L'assembly è inoltre case-insensitive e possono esserci uno o più spazi tra etichetta e istruzione e etichetta e argomento. 

Le seguenti istruzioni sono quindi tutte equivalenti:

```
ADD 3
Add 3
add 3 // Questo è un commento
ADD 3 // Aggiunte il valore della cella 3 all'accumulatore
aDD 3
```

Fatte queste premesse, dovrete quindi preparare un predicato ed una funzione che leggano un
file e, se non ci sono problemi, producano lo stato iniziale del LMC.

## IMPLEMENTAZIONI

### IMPLEMENTAZIONE IN PROLOG

L'implementazione del simulatore di LMC in Prolog deve rispettare le restrizioni elencate in questa sezione.

Lo stato del LMC deve essere rappresentato da un termine composto della seguente forma:
```prolog
state(Acc, Pc, Mem, In, Out, Flag).
```
nel caso non sia stata ancora eseguita una istruzione di halt.

Nel caso sia stata eseguita invece
lo stato deve essere rappresentato usando il funtore `halted_state` invece di state:
```prolog
halted_state(Acc, Pc, Mem, In, Out, Flag).
```

Nel termine composto gli argomenti sono i seguenti:
- Acc. Un numero tra 0 e 999 che rappresenta il valore contenuto nell'accumulatore.
- Pc. Un numero tra 0 e 99 che rappresenta il valore contenuto nel program counter.
- Mem. Una lista di 100 numeri tutti compresi tra 0 e 999 che rappresenta il contenuto della memoria del LMC.
- In. Una lista di numeri tra 0 e 999 che rappresenta la coda di input del LMC.
- Out. Una lista di numeri tra 0 e 999 che rappresenta la coda di output del LMC.
- Flag. Può assumere solo i valori flag e noflag, che indicano rispettivamente che il flag è presente o assente.

Per eseguire il codice del LMC deve essere presente il seguente predicato:
```prolog
one_instruction(State, NewState)
```
ove `State` e `NewState` sono stati del LMC rappresentati come descritto sopra ed il predicato è vero quando l'esecuzione di _una singola istruzione_ a partire da `State` porta allo stato `NewState`. 

Il predicato fallisce nei seguenti casi:
- Lo stato `State` è un `halting_state`, ovvero il sistema è stato arrestato e non può eseguire istruzioni.
- L'istruzione da eseguire è di input ma la coda di input è vuota.
- L'istruzione da eseguire non è valida.

In tutti gli altri casi una query della forma `one_instruction(State, X)` dove `State` è
fissato e `X` è una variabile deve avere successo e il valore di `X` deve essere l'unico stato che segue allo stato fornito dopo aver eseguito l'istruzione puntata dal program counter.

Si osservi il seguente esempio:
```prolog
?- one_instruction(state(32, 0, [101, 10, 0...0], [], [], noflag), X).
X = state(42, 1, [101, 10, 0...0], [], [], noflag)
```

Dove `0...0` rappresenta i 98 zero che servono ad ottenere una lista di 100 elementi. Nello stato iniziale fornito l'istruzione da eseguire è quella in posizione 0 (dato che il program counter ha valore 0). Questa istruzione ha valore numerico 101, ovvero è una istruzione di addizione che somma al valore contenuto nell'accumulatore il valore contenuto nella cella 1. In questo caso i due valori da sommare sono 32 (accumulatore) e 10 (cella di memoria 1). Il risultato, 42, diventa il nuovo valore dell'accumulatore ed il valore del program counter viene incrementato
di uno. Notare che, dato che il valore della somma non supera 999 il flag rimane spento (noflag).

Deve essere inoltre presente il seguente predicato:
```prolog
execution_loop(State, Out)
```
ove `State` rappresenta lo stato iniziale del LMC e `Out` la coda di output nel momento in cui viene raggiunto uno stato di stop (e quindi eseguita una istruzione di halt). Il predicato deve fallire nel caso l'esecuzione termini senza eseguire una istruzione di halt (ad esempio se si
incontra una istruzione non valida).

Infine, dovrete produrre due predicati per la gestione dell'assembly. Il primo è un predicato dal nome `lmc_load/2` che si preoccupa di leggere un file che contiene un codice assembler e che produce il contenuto "iniziale" della memoria sistema (una lista di 100 numeri tra 0 e 999).
```prolog
lmc_load(Filename, Mem)
```
dove `Filename` è il nome di un file e `Mem` è la memoria del sistema nel suo "stato iniziale".

Il secondo è un predicato dal nome `lmc_run/3` che si preoccupa di leggere un file che contiene un codice assembler, lo carica (con `lmc_load/2`), imposta la coda di input al valore fornito e produce un output che è il risultato dell'invocazione di `execution_loop/2`.
```prolog
?- lmc_run("path/to/test-assembly-42.lmc", [42], Output)
```
è un esempio della sua invocazione.

#### Suggerimenti
Per implementare al meglio (ed in modo semplice) il simulatore LMC in Prolog vi consigliamo
come minimo, di utilizzare il predicato `nth0/4`.

Un modo per generare uno "stato" iniziale è di usare il predicato di libreria `randseq/3`;
attenzione che il primo argomento non può essere superiore al secondo. Ad esempio:
```prolog
?- randseq(99, 99, Mem).
Mem = [33, 10, 64, 29, 62, 53, 98, 22, 36|...].
```
### IMPLEMENTAZIONE IN COMMON LISP

L'implementazione del simulatore di LMC in Common Lisp deve rispettare le restrizioni elencate
in questa sezione.

Lo stato del LMC deve essere rappresentato da una lista della seguente forma:
```lisp
(STATE  :ACC <Acc> 
        :PC <PC>
        :MEM <Mem>
        :IN <In>
        :OUT <Out>
        :FLAG <Flag>)
```
nel caso non sia stata ancora eseguita una istruzione di halt. Nel caso sia stata eseguita invece
lo stato deve essere rappresentato usando il funtore `HALTED-STATE` invece di `STATE`:
```lisp
(HALTED-STATE :ACC <Acc>
:PC <PC>
:MEM <Mem>
:IN <In>
:OUT <Out>
:FLAG <Flag>)
```

Nella lista qui sopra gli argomenti sono i seguenti:
- Acc. Un numero tra 0 e 999 che rappresenta il valore contenuto nell'accumulatore.
- Pc. Un numero tra 0 e 99 che rappresenta il valore contenuto nel program counter.
- Mem. Una lista di 100 numeri tutti compresi tra 0 e 999 che rappresenta il contenuto della
memoria del LMC.
- In. Una lista di numeri tra 0 e 999 che rappresenta la coda di input del LMC.
- Out. Una lista di numeri tra 0 e 999 che rappresenta la coda di output del LMC.
- Flag. Può assumere solo i valori flag e noflag, che indicano rispettivamente che il flag è
presente o assente.

Per eseguire il codice del LMC deve essere definita la seguente funzione:

`one-instruction State ==> NewState`

ove `State` e `NewState` sono stati del LMC rappresentati come descritto sopra ed il predicato è
vero quando l'esecuzione di _una singola istruzione_ a partire da `State` porta allo stato
`NewState`. Il predicato fallisce nei seguenti casi:
- Lo stato State è un `HALTING-STATE`, ovvero il sistema è stato arrestato e non può eseguire
istruzioni.
- L'istruzione da eseguire è di input ma la coda di input è vuota.
- L'istruzione da eseguire non è valida.

In tutti gli altri casi l'invocazione della funzione `(one-instruction State)` dove `State` è fissato ritorna un nuovo stato `NewState` che deve essere l'unico stato che segue allo stato fornito dopo aver eseguito l'istruzione puntata dal program counter.

Si osservi il seguente esempio:
```lisp
?- (one-instruction '(state :acc 32
                            :pc 0
                            :mem (101 10 0...0)
                            :in ()
                            :out ()
                            :flag noflag))

(STATE :ACC 42 :PC 1 :MEM (101 10 0...0) :IN () :OUT () :FLAG NOFLAG)
```
Dove `0...0` rappresenta i 98 zero che servono ad ottenere una lista di 100 elementi. Nello stato iniziale fornito l'istruzione da eseguire è quella in posizione 0 (dato che il program counter ha valore 0). Questa istruzione ha valore numerico 101, ovvero è una istruzione di addizione che somma al valore contenuto nell'accumulatore il valore contenuto nella cella 1. In questo caso i due valori da sommare sono 32 (accumulatore) e 10 (cella di memoria 1). Il risultato, 42, diventa il nuovo valore dell'accumulatore ed il valore del program counter viene incrementato
di uno. Notare che, dato che il valore della somma non supera 999 il flag rimane spento (noflag).

Deve essere inoltre presente il seguente predicato:

`execution-loop State ==> Out`

ove `State` rappresenta lo stato iniziale del LMC e `Out` la coda di output nel momento in cui viene raggiunto uno stato di stop (e quindi eseguita una istruzione di halt). La funzione deve generare un errore nel caso l'esecuzione termini senza eseguire una istruzione di halt (ad
esempio se si incontra una istruzione non valida).


Infine, dovrete produrre due funzioni per la gestione dell'assembly. La prima è una funzione dal nome lmc-load che si preoccupa di leggere un file che contiene un codice assembler e che produce il contenuto "iniziale" della memoria del sistema (una lista di 100 numeri tra 0 e 999).

`lmc-load Filename ==> Mem`

dove `Filename` è il nome di un file e `Mem` è il contenuto della memoria nello "stato iniziale" del sistema.

La seconda è una funzione dal nome `lmc-run` che si preoccupa di leggere un file che contiene un codice assembler, lo carica (con `lmc-load`), inizializza la coda di input al valore fornito, e produce un output che è il risultato dell'invocazione di `execution-loop`.
```lisp
?- (lmc-run "path/to/test-assembly-42.lmc", '(9))
;;; Output
```
è un esempio della sua invocazione.

#### Suggerimenti
Per implementare al meglio (ed in modo semplice) il simulatore LMC in Lisp vi permettiamo di usare la funzione `setf` in combinazione con la funzione `nth`.

Un modo per generare uno "stato" iniziale è di usare la funzione `make-list`:
```lisp
?- (make-list 100 :initial-element 0)
(0 0 0 0 ...) ; 100 zeri.
```

## CREDITI

| [<img src="https://avatars.githubusercontent.com/u/31136677?v=4" width="100px;" alt="dlcgold"/><br /><sub><b>dlcgold</b></sub>](https://github.com/dlcgold) | [<img src="https://avatars.githubusercontent.com/u/4183824?v=4" width="100px;" alt="derogab"/><br /><sub><b>derogab</b></sub>](https://github.com/derogab) | 
| :---: | :---: |