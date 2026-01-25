%Bernasconi Giorgio Luigi   Maria   885948



%DB-base di conoscenza

%Funtori riservati per le regex
reserved_functor(c).
reserved_functor(a).
reserved_functor(z).
reserved_functor(o).


% Questi tre predicati rappresentano l'automa in memoria, informa prolog
% che: nfsa_init, nfsa_final e nfsa_delta, saranno creati solo
% dinamicamente durante l'esecuzione
:- dynamic nfsa_init/2.
:- dynamic nfsa_final/2.
:- dynamic nfsa_delta/4.

%permette di definire come sempre true le epsilon transition
epsilon(eps).



% Funzioni dell'interfaccia, pensate per l'uso esterno

%is_regex/1
%is_regex funzione pensata sia per uso esterno sia per uso interno da
%parte del predicato "nfsa_compile_regex/2"

% Prima viene verificato che RE sia un atomo prolog, escludendo
%però eps
is_regex(RE) :-
    atomic(RE),
    epsilon(Eps),
    RE \== Eps,
    !.

%Verifica se RE è vera per compund/1 e che sia uno dei funtori
%riservati, se e' riservato, controlla che la forma sia corretta
is_regex(RE) :-
    compound(RE),
    functor(RE, F, Arity),
    reserved_functor(F),
    !,
    is_reserved_regex(F, Arity, RE).

%Accettautte le compound come simboli dell'alfabeto
is_regex(RE) :-
    compound(RE),
    functor(RE, F, _),
    \+ reserved_functor(F),
    !.


%nfsa_conpile_regex/2

% nfsa_compile_regex crea un NFSA corrispondente alla RE fornita e lo
% inserisce nel DB, eliminando prima eventuali definizioni precedenti
nfsa_compile_regex(FA_Id, RE) :-
    ground(FA_Id),
    is_regex(RE),
    !,
    nfsa_delete(FA_Id),
    compile_frag(FA_Id, RE, S, F),
    assertz(nfsa_init(FA_Id, S)),
    assertz(nfsa_final(FA_Id, F)),
    !.


%nfsa_delete/1

%nfsa_delete elimina l'automa corrispondente all'ID fornito
nfsa_delete(FA_Id) :-
    retractall(nfsa_init(FA_Id, _)),
    retractall(nfsa_final(FA_Id, _)),
    retractall(nfsa_delta(FA_Id, _, _, _)).


%nfsa_delete_all/0

%nfsa_delete_all rimuove dal database dinamico tutti gli automi
nfsa_delete_all :-
    retractall(nfsa_init(_, _)),
    retractall(nfsa_final(_, _)),
    retractall(nfsa_delta(_, _, _, _)).


% nfsa_recognize/2

% nfsa_recognize verifica se la lista in input porta a stato finale
% nell'automa con id corrispondente, usando epsilon-closure e move
nfsa_recognize(FA_Id, Input) :-
    is_list(Input),
    input_symbols_ok(Input),
    !,
    nfsa_init(FA_Id, S0),
    epsilon_closure(FA_Id, [S0], C0),
    recognize_from(FA_Id, C0, Input, CF),
    has_final_state(FA_Id, CF).


% Funzioni ausiliarie impiegate nell'uso interno

%Gestione dei costruttori speciali regex

%z(<re>)
is_reserved_regex(z, 1, RE):-
    arg(1, RE, X),
    is_regex(X).

%o(<re>)
is_reserved_regex(o, 1, RE):-
    arg(1, RE, X),
    is_regex(X).

%c(<re1>, <re2>, ..., <rek>)
is_reserved_regex(c, Arity, RE):-
    Arity >= 1,
    RE =.. [c|Args],
    all_regex(Args).

%a(<re1>, <re2>, ..., <rek>)
is_reserved_regex(a, Arity, RE):-
    Arity >= 1,
    RE =.. [a|Args],
    all_regex(Args).

%Verifica che tutti gli elementi siano regex
all_regex([]).
all_regex([X|Xs]):-
    is_regex(X),
    all_regex(Xs).


%compilazione a frammenti


%caso z, chiusura di Kleene
compile_frag(FA, z(R), S, F) :-
    !,
    fresh_state(S),
    fresh_state(F),
    compile_frag(FA, R, S1, F1),
    epsilon(Eps),
    assertz(nfsa_delta(FA, S, Eps, F)),
    assertz(nfsa_delta(FA, S, Eps, S1)),
    assertz(nfsa_delta(FA, F1, Eps, S1)),
    assertz(nfsa_delta(FA, F1, Eps, F)).

%caso o, "1 o più"
compile_frag(FA, o(R), S, F) :-
    !,
    fresh_state(S),
    fresh_state(F),
    compile_frag(FA, R, S1, F1),
    epsilon(Eps),
    assertz(nfsa_delta(FA, S, Eps, S1)),  % entra nel corpo (obbligatorio)
    assertz(nfsa_delta(FA, F1, Eps, S1)), % loop
    assertz(nfsa_delta(FA, F1, Eps, F)).  % esci

%caso c, concatenazione
compile_frag(FA, RE, S, F) :-
    RE =.. [c|Args],
    Args = [_|_],
    !,
    compile_concat(FA, Args, S, F).

%caso a, alternativa
compile_frag(FA, RE, S, F) :-
    RE =.. [a|Args],
    Args = [_|_],
    !,
    compile_alt(FA, Args, S, F).

%caso con SIMBOLO, atomic o compound non riservato
compile_frag(FA, RE, S, F) :-
    !,
    fresh_state(S),
    fresh_state(F),
    assertz(nfsa_delta(FA, S, RE, F)).

%CONCAT

%compile_concat compila una lista di regex in sequenza
%Ogni frammento viene collegato al successivo con una
%epsilon-transition
compile_concat(FA, [R|Rs], S, F) :-
    compile_frag(FA, R, S, F0),
    compile_concat_rest(FA, Rs, F0, F).

compile_concat_rest(_FA, [], CurrentEnd, CurrentEnd).
compile_concat_rest(FA, [R|Rs], CurrentEnd, FinalEnd) :-
    compile_frag(FA, R, Snext, Fnext),
    epsilon(Eps),
    assertz(nfsa_delta(FA, CurrentEnd, Eps, Snext)),
    compile_concat_rest(FA, Rs, Fnext, FinalEnd).

%ALT

%compile_alt compila una lista di alternative creando un nodo di split
%ed un nodo di merge, collegati con epsilon-transition ai vari rami
compile_alt(FA, [R|Rs], S, F) :-
    fresh_state(S),
    fresh_state(F),
    epsilon(Eps),
    compile_alt_branches(FA, [R|Rs], S, F, Eps).

compile_alt_branches(_FA, [], _S, _F, _Eps).
compile_alt_branches(FA, [R|Rs], S, F, Eps) :-
    compile_frag(FA, R, Sr, Fr),
    assertz(nfsa_delta(FA, S, Eps, Sr)),   % split
    assertz(nfsa_delta(FA, Fr, Eps, F)),   % merge
    compile_alt_branches(FA, Rs, S, F, Eps).

%fresh_state crea nuovi stati
fresh_state(Q) :-
    gensym(q, Q).

%Verifica che i "simboli" siano dell'alfabeto
input_symbols_ok([]).
input_symbols_ok([X|Xs]) :-
    ( atomic(X) ; compound(X) ),
    epsilon(Eps),
    X \== Eps,
    input_symbols_ok(Xs).

% Simulazione NFSA
%recognize_from simula l'automa consumando l'input simbolo per simbolo
recognize_from(_FA, CurrStates, [], CurrStates) :- !.
recognize_from(FA, CurrStates, [Sym|Rest], FinalStates) :-
    move(FA, CurrStates, Sym, Next0),
    Next0 \= [],
    epsilon_closure(FA, Next0, Next),
    recognize_from(FA, Next, Rest, FinalStates).

%has_final_state verifica che tra gli stati correnti ci sia uno stato finale
has_final_state(FA, States) :-
    member(S, States),
    nfsa_final(FA, S),
    !.


%move calcola gli stati raggiungibili con transizioni etichettate con il
%simbolo corrente
move(FA, States, Sym, NextStates) :-
    findall(To,
            ( member(From, States),
              nfsa_delta(FA, From, Label, To),
              Label == Sym
            ),
            Tos),
    sort(Tos, NextStates).


%epsilon_closure calcola la chiusura epsilon di un insieme di stati
%Parte dai Seeds e visita tutti gli stati raggiungibili con sole epsilon
epsilon_closure(FA, Seeds, Closure) :-
    sort(Seeds, SeedsSet),
    eps_bfs(FA, SeedsSet, SeedsSet, Closure).

%eps_bfs esegue una BFS sugli stati raggiungibili tramite epsilon-transition
%Queue e' la coda BFS, Vis e' l'insieme degli stati gia' visitati
eps_bfs(_FA, [], Vis, Vis) :- !.
eps_bfs(FA, [S|Queue], Vis, Closure) :-
    findall(T, nfsa_delta(FA, S, eps, T), Ts),
    filter_new(Ts, Vis, New),
    append(Queue, New, Queue1),
    append(Vis, New, Vis1),
    eps_bfs(FA, Queue1, Vis1, Closure).

%filter_new rimuove dalla lista i nodi gia' visitati
%Restituisce solo quelli nuovi da aggiungere alla BFS
filter_new([], _Visited, []).
filter_new([X|Xs], Visited, New) :-
    ( memberchk(X, Visited) ->
        filter_new(Xs, Visited, New)
    ;
        New = [X|Rest],
        filter_new(Xs, [X|Visited], Rest)
    ).