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

reduce_fill_path(FT1 / FT2, NFT1 / NFT2, T) :-
    atom(FT1) ->
        NFT1 =.. [FT1, T], 
        reduce_fill_path(FT2, NFT2, T)
    ; segment_type(FT1, T) ->
        NFT1 = FT1,
        reduce_fill_path(FT2, NFT2, _).

reduce_fill_path(FT, FT, T) :-
    FT \= _ / _,
    segment_type(FT, T).

segment_type(F, T) :- F =.. [_, T].
segment_type(functor(_, _, _ / T), T).


inner_reduce(FT1 / FT2, F, R) :-
    map(FT1, inner_reduce(FT2), F, FR),
    inner_reduce(FT1, FR, R).

inner_reduce(FT, F, R) :-
    FT =.. [_,T],
    empty(T, E),
    fold(FT, combine(T), E, F, R).

inner_reduce(functor(F, Arity, Field / _), Fun, V) :-
    functor(Fun, F, Arity),
    Fun =.. [F | Args],
    nth1(Field, Args, V).

reduce(FT, F, R) :-
    rebalance_path(FT, FT2),
    reduce_fill_path(FT2, FT3, _),
    inner_reduce(FT3, F, R).
