:- module(folded, [folded/5]).

:- use_module(mapped).
:- use_module(utils/dsl).
:- use_module(isa).

:- meta_predicate folded(?, 3, ?, ?, ?).
/**
 * folded(+FoldableType, :Predicate, +Zero, +Foldable, ?Fold) is nondet
 * 
 * Folds Foldable using Predicate, with initial accumulator value Zero.
 */
folded(T, P, D, F, R) :-
    translate(T, TT), !,
    folded_(TT, P, D, F, R).
    
:- meta_predicate folded_(?, 3, ?, ?, ?).
folded_(F1/F2, P, D, F, R) :-
    mapped:mapped_(F1, folded:folded_(F2, P, D), F, FR),
    folded_(F1, P, D, FR, R).

folded_(F1 ; F2, Pred, Zero, F, R) :-
    isa:isa_(F1, F), !,
    folded_(F1, Pred, Zero, F, R)
    ; folded_(F2, Pred, Zero, F, R).

folded_(list, P, D, L, R) :- foldl(P, L, D, R).

folded_(id, P, Z, V, R) :- 
    call(P, Z, V, R).

folded_(dict(S, Fields), Pred, Zero, Dict, Result) :-
    is_dict(Dict, S),
    foldl(dict_folded_(Dict, Pred), Fields, Zero, Result).

folded_(FT:_, Pred, Zero, Dict, Result) :-
    folded_(FT, Pred, Zero, Dict, Result).

folded_(functor(F, Arity, Fields), Pred, Zero, Fun, Result) :-
    functor(Fun, F, Arity),
    foldl(functor_folded_(Fun, Pred), Fields, Zero, Result).

dict_folded_(Dict, Pred, Field, Acc, NewAcc) :-
    ( integer(Field) ; atom(Field) ), !,
    get_dict(Field, Dict, Elem), 
    call(Pred, Acc, Elem, NewAcc).

dict_folded_(Dict, Pred, Field / FieldType, Acc, NewAcc) :-
    get_dict(Field, Dict, Inner), 
    folded_(FieldType, Pred, Acc, Inner, NewAcc).

functor_folded_(Fun, Pred, Field, Acc, NewAcc) :-
    integer(Field), !,
    Fun =.. [_|Args],
    nth1(Field, Args, Elem),
    call(Pred, Acc, Elem, NewAcc).

functor_folded_(Fun, Pred, Field / FieldType, Acc, NewAcc) :-
    integer(Field), !,
    Fun =.. [_|Args],
    nth1(Field, Args, Elem),
    folded_(FieldType, Pred, Acc, Elem, NewAcc).
