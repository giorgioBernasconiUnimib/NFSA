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


%Funtori riservati per le regex
reserved_functor(c).
reserved_functor(a).
reserved_functor(z).
reserved_functor(o).
