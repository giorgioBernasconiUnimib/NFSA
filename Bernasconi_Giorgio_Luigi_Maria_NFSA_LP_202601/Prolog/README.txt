Bernasconi Giorgio Luigi Maria 885948

-----------------------------------------------------------------------
Parametri ricorrenti

FA_Id: identificatore dell'automa NFSA nel database dinamico
RE: espressione regolare (regex) nel formato richiesto dalla specifica
Input: lista di simboli di input (atomic/1 o compound/1) da riconoscere

S: stato iniziale di un frammento NFSA
F: stato finale di un frammento NFSA
States: lista di stati correnti durante la simulazione

Eps: simbolo riservato per le epsilon-transition

!!  Nota importante  !!
Per evitare ambiguità, eps è riservato esclusivamente per le epsilon-transition, quindi non è ammesso come simbolo dell'alfabeto nè nelle regex nè nelle liste Input passate a nfsa_recognize/2.

-----------------------------------------------------------------------

Uso delle funzioni

Queste sono le funzioni dell'interfaccia, ovvero quelle pensate per essere accessibili dall'esterno.

E' fondamentale che gli input forniti siano corretti, altrimenti la query darà risposta "false". In particolare:
- FA_Id deve essere ground (un identificatore senza variabili)
- Input deve essere una lista
- Ogni simbolo di Input deve essere atomic/1 oppure compound/1
- eps non è ammesso come simbolo di input perchè riservato a epsilon

- is_regex(RE):
Verifica se il termine RE è una regex valida secondo la specifica.
Accetta:
1) atomic/1 come simbolo, escludendo eps.
2) compound/1 se e solo se:
  - ha un funtore riservato tra {c,a,z,o} e rispetta la forma
       prevista (argomenti tutti regex), oppure
  - ha un funtore non riservato e viene trattato come simbolo
       dell'alfabeto.

- nfsa_compile_regex(FA_Id, RE):
Compila la regex RE in un automa NFSA identificato da FA_Id e lo inserisce nel database dinamico.
Se esiste già un automa con lo stesso id, viene prima eliminato.
La compilazione segue una costruzione a frammenti (stile Thompson), creando stati nuovi con gensym/2 e salvando le transizioni tramite nfsa_delta/4.

- nfsa_recognize(FA_Id, Input):
Simula l'automa identificato da FA_Id sulla lista di simboli Input.
La procedura:
  1) controlla la correttezza di Input (inclusa l'esclusione di eps).
  2) calcola la epsilon-closure dello stato iniziale.
  3) consuma l'input simbolo per simbolo eseguendo move e poi epsilon-closure.
  4) accetta se tra gli stati raggiunti al termine dell'input esiste almeno uno stato finale.

- nfsa_delete(FA_Id):
Elimina dal database dinamico l'automa con identificatore FA_Id, rimuovendo fatti nfsa_init/2, nfsa_final/2 e nfsa_delta/4 associati.

- nfsa_delete_all:
Elimina dal database dinamico tutti gli automi salvati.


-----------------------------------------------------------------------

Funzioni ausiliarie

Queste funzioni non sono pensate per l'utilizzo esterno alla libreria.
Si limitano a fornire logica addizionale necessaria per la validazione delle regex, la compilazione e la simulazione dell'automa.

Validazione e supporto alle regex:

- is_reserved_regex(F, Arity, RE):
Controlla che una regex con funtore riservato sia ben formata.
I costruttori riservati sono:
z(<re>) : chiusura di Kleene (0 o più)
o(<re>) : uno o piu
c(<re1>,...,<rek>) : concatenazione (k >= 1)
a(<re1>,...,<rek>) : alternativa (k >= 1)

- all_regex(List):
Verifica che tutti gli elementi di List siano regex valide.

Compilazione a frammenti:

- compile_frag(FA, RE, S, F):
Compila una regex RE in un frammento NFSA, restituendo uno stato iniziale S e uno stato finale F. Le transizioni vengono salvate nel database tramite nfsa_delta/4. Gestisce i casi:
z/1, o/1, c/k, a/k, e il caso "simbolo".

- compile_concat(FA, Args, S, F):
Compila la concatenazione di una lista di regex, collegando i frammenti tramite epsilon-transition. Viene chiamato dal caso "c".

- compile_alt(FA, Args, S, F):
Compila l'alternativa tra una lista di regex, creando uno stato di split e uno di merge, collegati ai rami tramite epsilon-transition. Viene chiamato dal caso "a"

Gestione stati e input:

- fresh_state(Q):
Genera un nuovo identificatore di stato con gensym/2.

- input_symbols_ok(Input):
Verifica che ogni elemento di Input sia un simoblo ammesso (atomic/1 o compound/1) e che non sia eps.

Simulazione dell'automa:

- recognize_from(FA, CurrStates, Input, FinalStates):
Consuma la lista Input a partire dall'insieme di stati CurrStates.
Ad ogni passo esgue:
1) move con il simbolo corrente.
2) epsilon_closure sul risultato di move.
Restituisce FinalStates alla fine dell'input.

- has_final_state(FA, States):
Verifica se tra gli stati in States esiste almeno uno stato finale.

- move(FA, States, Sym, NextStates):
Calcola l'insieme NextStates degli stati raggiungibili da uno qualsiasi
stato in States tramite una transizione etichettata esattamente Sym.

- epsilon_closure(FA, Seeds, Closure):
Calcola la chiusura epsilon dell'insieme Seeds, restituendo Closure.

- eps_bfs(FA, Queue, Visited, Closure):
Esegue una BFS sugli stati raggiungibili con sole epsilon-transition. Queue è la coda BFS, Visited l'insieme degli stati gia' visitati.

- filter_new(Ts, Visited, New):
Filtra gli stati in Ts rimuovendo quelli gia' visitati, restituendo la lista dei soli stati nuovi.

Rappresentazione nel database dinamico:

- nfsa_init(FA_Id, State):
Stato iniziale dell'automa FA_Id

- nfsa_final(FA_Id, State):
Stato finale dell'automa FA_Id

- nfsa_delta(FA_Id, From, Label, To):
Transizione dell'automa, Fa_Id l'identificatore, From e To gli stati coinvolti e Label è l'etichetta della transizione

- epsilon(eps):
Il simbolo RISERVATO delle transizioni epsilon
