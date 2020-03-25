:- module(reduce, [reduce/3]) .

:- use_module(semigroup).
:- use_module(empty).
:- use_module(fold).
:- use_module(functor).

rebalance_path(A / B / C, NP) :-
    rebalance_path(A / (B/C), NP).
rebalance_path(A / B, A / B) :-
    A \= _ / _.
rebalance_path(A, A) :-
    A \= _ / _.

reduce_fill_path(F1 / F2, F1 / NP, T) :- 
    F1 =.. [_, T],
    reduce_fill_path(F2, NP, _).

reduce_fill_path(F1 / F2, NF1 / NP, T) :-
    atom(F1),
    reduce_fill_path(F2, NP, T),
    NF1 =.. [F1, T].

reduce_fill_path(P, P, T) :-
    dif(P, _ / _),
    P =.. [_, T].


inner_reduce(FT1 / FT2, F, R) :-
    map(FT2, inner_reduce(FT2), F, FR),
    inner_reduce(FT1, FR, R).

inner_reduce(FT, F, R) :-
    FT =.. [_,T],
    empty(T, E),
    fold(FT, combine(T), E, F, R).

reduce(FT, F, R) :-
    rebalance_path(FT, FT2),
    reduce_fill_path(FT2, FT3, _),
    inner_reduce(FT3, F, R).
