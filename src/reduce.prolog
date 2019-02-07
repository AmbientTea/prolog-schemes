:- module(reduce, [reduce/3]) .

:- use_module(monoid).
:- use_module(fold).
:- use_module(functor).

reduce(FT1 / FT2, F, R) :-
    map(FT2, reduce(FT2), F, FR),
    reduce(FT1, FR, R).

reduce(FT, F, R) :-
    FT =.. [_,T],
    empty(T, E),
    fold(FT, combine(T), E, F, R).
