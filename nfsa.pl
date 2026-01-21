%Bernasconi Giorgio Luigi Maria 885948

% Funzioni dell'interfaccia, pensate per l'uso esterno


%is_regex funzione pensata sia per uso esterno sia per uso interno da
%parte del predicato "nfsa_compile_regex/2"

% Prima viene verificato che RE sia un atomo prolog, se soddisfa
% atomic/1 è una regex
is_regex(RE) :-
    atomic(RE),
    !.

%Verifica se RE è vera per compund/1, se lo è deve verificare prima che
%functor non abbia uno riservato
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
    \+ reserved_functor(F).


% nfsa_compile_regex crea un NFSA corrispondente alla RE fornita e lo
% inserisce nel DB
nfsa_compile_regex(FA_Id, RE) :-
    ground(FA_Id),
    is_regex(RE),
    !,
    %nfsa_delete(FA_Id),  %Rimuovere il "%" se si vuole riusare ID
    compile_frag(FA_Id, RE, S, F),
    assertz(nfsa_init(FA_Id, S)),
    assertz(nfsa_final(FA_Id, F)).

%nfsa_delete elimina l'automa corrispondente all'ID fornito
nfsa_delete(FA_Id) :-
    retractall(nfsa_init(FA_Id, _)),
    retractall(nfsa_final(FA_Id, _)),
    retractall(nfsa_delta(FA_Id, _, _, _)).

%rimuove dal database dinamico tutti gli
nfsa_delete_all :-
    retractall(nfsa_init(_, _)),
    retractall(nfsa_final(_, _)),
    retractall(nfsa_delta(_, _, _, _)).



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
    Arity >=1,
    RE =.. [c|Args],
    all_regex(Args).

%a(<re1>, <re2>, ..., <rek>)
is_reserved_regex(a, Arity, RE):-
    Arity >=1,
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

%
compile_concat(FA, [R|Rs], S, F) :-
    compile_frag(FA, R, S, F0),
    compile_concat_rest(FA, Rs, F0, F).

%
compile_concat_rest(_FA, [], CurrentEnd, CurrentEnd).
compile_concat_rest(FA, [R|Rs], CurrentEnd, FinalEnd) :-
    compile_frag(FA, R, Snext, Fnext),
    epsilon(Eps),
    assertz(nfsa_delta(FA, CurrentEnd, Eps, Snext)),
    compile_concat_rest(FA, Rs, Fnext, FinalEnd).

%ALT

%
compile_alt(FA, [R|Rs], S, F) :-
    fresh_state(S),
    fresh_state(F),
    epsilon(Eps),
    compile_alt_branches(FA, [R|Rs], S, F, Eps).

%
compile_alt_branches(_FA, [], _S, _F, _Eps).
compile_alt_branches(FA, [R|Rs], S, F, Eps) :-
    compile_frag(FA, R, Sr, Fr),
    assertz(nfsa_delta(FA, S, Eps, Sr)),   % split
    assertz(nfsa_delta(FA, Fr, Eps, F)),   % merge
    compile_alt_branches(FA, Rs, S, F, Eps).

%fresh_state crea nuovi stati
fresh_state(Q) :-
    gensym(q, Q).

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
