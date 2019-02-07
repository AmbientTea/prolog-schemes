:- module(fold, [fold/5]).

:- use_module(functor).

:- meta_predicate fold(?, 3, ?, ?, ?).
fold(F1/F2, P, D, F, R) :-
    map(F1, fold(F2, P, D), F, FR),
    fold(F1, P, D, FR, R).

fold(list(_), P, D, L, R) :- foldl(P, L, D, R).
fold(list, P, D, L, R) :- fold(list(_), P, D, L, R).
