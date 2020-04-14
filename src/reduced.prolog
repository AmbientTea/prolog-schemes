:- module(reduced, [reduced/3]) .

:- use_module(combined).
:- use_module(empty).
:- use_module(folded).
:- use_module(mapped).

rebalance_path(A / B / C, NP) :-
    rebalance_path(A / (B/C), NP).
rebalance_path(A / B, A / B) :-
    A \= _ / _.
rebalance_path(A, A) :-
    A \= _ / _.

reduced_fill_path(FT1 / FT2, NFT1 / NFT2, T) :-
    atom(FT1) ->
        NFT1 =.. [FT1, T], 
        reduced_fill_path(FT2, NFT2, T)
    ; segment_type(FT1, T) ->
        NFT1 = FT1,
        reduced_fill_path(FT2, NFT2, _).

reduced_fill_path(FT, FT, T) :-
    FT \= _ / _,
    segment_type(FT, T).

segment_type(F, T) :- F =.. [_, T].
segment_type(functor(_, _, _ / T), T).


inner_reduced(FT1 / FT2, F, R) :-
    mapped(FT1, inner_reduced(FT2), F, FR),
    inner_reduced(FT1, FR, R).

inner_reduced(FT, F, R) :-
    FT =.. [_,T],
    empty(T, E),
    folded(FT, combined(T), E, F, R).

inner_reduced(functor(F, Arity, Field / _), Fun, V) :-
    functor(Fun, F, Arity),
    Fun =.. [F | Args],
    nth1(Field, Args, V).

reduced(FT, F, R) :-
    rebalance_path(FT, FT2),
    reduced_fill_path(FT2, FT3, _),
    inner_reduced(FT3, F, R).
