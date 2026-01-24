Bernasconi Giorgio Luigi Maria 885948

-----------------------------------------------------------------------

Parametri ricorrenti

RE: espressione regolare (regex) nel formato Lisp richiesto dalla specifica, vedi pdf

FA: automa NFSA rappresentato tramite la struttura nfsa, creata da nfsa-compile-regex

Input: lista Lisp di simboli di input (S-expressions), da riconoscere

S, F: stati iniziale e finale di un frammento NFSA durante la compilazione

States: lista di stati correnti durante la simulazione

Delta: insieme delle transizioni dell'automa, rappresentate come liste di tre elementi; "from" e "to" gli stati coinvolti, lable l'etichetta

Epsilon-label: etichetta riservata per le epsilon-transition. In Lisp, per evitare ambiguita' con l'alfabeto, viene generata con gensym, questo in caso di una ricompilazione del file DOPO aver generato e memorizzato con "defparameter" potrebbe portare al non riconoscimento delle transizioni epsilon. Ho ritenuto fosse un buon compromesso per non riservare una parola dell'alfabeto come invece faccio in prolog

-----------------------------------------------------------------------

Rappresentazione dell'automa

-L'automa viene rappresentato tramite una struct:

(defstruct nfsa
  init
  finals
  delta)

-init: 
stato iniziale dell'automa

-finals:
lista di stati finali

-delta:
lista di transizioni, dove ogni transizione ha forma (From Label To)

-Gli stati sono simboli generati con gensym (per garantire unicità).
Le label delle transizioni possono essere:
  -un simbolo di input appartenente all'alfabeto
  -la epsilon-label, usata per le transitioni epsilon


-----------------------------------------------------------------------

Uso delle funzioni

Queste sono le funzioni dell'interfaccia, ovvero quelle pensate per essere accessibili dall'esterno.

- (is-regex RE):
Verifica se RE è una regex valida.
Accetta:
1) qualunque atomo come simbolo dell'alfabeto.
2) liste nella forma:
   (z <re>)    chiusura di Kleene, 0 o più
   (o <re>)    1 o più
   (c <re1> ... <rek>) concatenazione, k >= 1
   (a <re1> ... <rek>) alternativa, k >= 1
Se la forma è scorretta o l'operatore è riservato (ma non nel formato previsto, es: (z <re> <re>)), restituisce NIL.

- (nfsa-compile-regex RE):
Se RE è una regex valida, costruisce e restituisce un automa NFSA equivalente, altrimenti restituisce NIL.
La compilazione è effettuata tramite costruzione a frammenti, creando stati nuovi con gensym e collegando i frammenti con epsilon-transition.

- (nfsa-recognize FA Input):
Simula l'automa FA sulla lista Input.
Restituisce T se Input è riconosciuto, altrimenti NIL, se FA non ha una struttura valida, segnala un errore.
La procedura di simulazione, dettagliata meglio nelle funzioni ausiliarie della sezione "simulazione", è:
  1) calcolo della epsilon-closure dello stato iniziale.
  2) per ogni simbolo Sym in Input:
     - calcolo di move sugli stati correnti con Sym
     - calcolo della epsilon-closure del risultato
  3) l'input è accettato se, alla fine, l'insieme di stati correnti contiene almeno uno stato finale.


-----------------------------------------------------------------------

Funzioni ausiliarie

Queste funzioni non sono pensate per l'utilizzo esterno, si limitano a fornire logica addizionale necessaria per la validazione delle regex, la compilazione e la simulazione dell'automa.

Validazione e supporto:

- reserved-regex-op-p:
Ritorna T se un simbolo è uno degli operatori riservati delle regex: c, a, z, o.

- proper-list-p:
Verifica che un oggetto sia una lista propria, escludendo dotted pair e liste cicliche, che altrimenti avrebbero creato iterazioni infinite e errori

Compilazione:

- compile-frag:
Compila una regex in un frammento NFSA e restituisce tre valori:ù
stato iniziale, stato finale e lista di transizioni.
Gestisce i casi:
z, o, c (vedi compile-concat), a (vedi compile-alt) e simbolo dell'alfabeto. 
Le epsilon-transition vengono etichettate con epsilon-label, motivato a inizio file

- compile-concat:
Compila una lista di regex in concatenazione.
Collega il finale di un frammento all'inizio del successivo con una epsilon-transition.

- compile-alt:
Compila una lista di regex con alternativa. 
Crea uno stato di split e uno stato di merge, collegando ogni ramo con epsilon-transition.

- fresh-state:
Genera un nuovo simbolo di stato con gensym.

Simulazione:

- nfsa-ok-p:
Verifica che un oggetto abbia la struttura minima di un automa NFSA generato da nfsa-compile-regex ("(From Label To)").

- move:
Calcola gli stati raggiungibili da un insieme di stati, consumando un singolo simbolo di input (escludendo epsilon-transition).
Gli stati sono confrontati con EQ (gensym), i simboli di input con EQUAL (S-expressions).

- epsilon-closure:
Calcola la chiusura epsilon di un insieme di stati usando BFS, seguendo solo transizioni con label uguale a epsilon-label.

- has-final-state-p:
Ritorna T se l'insieme di stati correnti contiene almeno uno stato finale.


-----------------------------------------------------------------------

Scelte implementative e motivazioni

- Uguaglianza (vedi move): 
Gli stati sono simboli unici (gensym), per cui EQ è sufficiente e dovrebbe essere più veloce.
I simboli di input possono essere strutture (liste, numeri, stringhe), quindi vengono confrontati con EQUAL.

- Ricaricamento del file:
Durante alcuni test ho notato questa problematica che può verificarsi. La epsilon-label è generata a load-time. Se un automa viene costruito, e successivamente si ricarica il file, la nuova epsilon-label potrebbe essere diversa da quella salvata nelle transizioni dell'automa creato prima del reload. Come dicevo però all'inizio del file ho ritenuto fosse un compromesso accettabile dato che così non dovevo richiedere di riservare eps come in prolog.
