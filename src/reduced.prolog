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
    mapped(FT1, reduced_(FT2), F, FR),
    reduced_(FT1, FR, R).

reduced_(FT, F, R) :-
    FT =.. [_,T],
    empty(T, E),
    folded(FT, combined(T), E, F, R).

reduced_(functor(F, Arity, [Field / _]), Fun, V) :- 
    functor(Fun, F, Arity),
    Fun =.. [F | Args],
    nth1(Field, Args, V).

reduced_(FT:T, F, V) :-
    empty:empty_(T, E),
    folded:folded_(FT, combined:combined_(T), E, F, V).
