:- module(reduced, [reduced/3]) .

:- use_module(utils/dsl).

:- use_module(combined).
:- use_module(empty).
:- use_module(folded).
:- use_module(mapped).

reduced(FT, F, R) :-
    translate(FT, FT2),
    reduced_(FT2, F, R).

reduced_(FT1 / FT2, F, R) :-
    mapped:mapped_(FT1, reduced:reduced_(FT2), F, FR),
    reduced_(FT1, FR, R).

reduced_(FT:T, F, V) :-
    empty:empty_(T, E),
    folded:folded_(FT, combined:combined_(T), E, F, V).
