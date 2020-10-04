:- module(reduced, [reduced/3]) .

:- use_module(utils/dsl).

:- use_module(combined).
:- use_module(empty).
:- use_module(folded).
:- use_module(mapped).


reduced_fill_path(FT1 / FT2, FT1 / NFT2, T) :-
    segment_type(FT1, T),
    ( var(T) -> T = IT ; true ),
    reduced_fill_path(FT2, NFT2, IT).

reduced_fill_path(FT, FT, T) :-
    FT \= _ / _,
    segment_type(FT, T).

segment_type(F, T) :- F =.. [_, T].
segment_type(functor(_, _, _ / T), T).

reduced(FT, F, R) :-
    translate(FT, FT2),
    reduced_fill_path(FT2, FT3, _),
    reduced_(FT3, F, R).

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
