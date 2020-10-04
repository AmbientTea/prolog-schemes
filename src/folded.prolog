:- module(folded, [folded/5]).

:- use_module(mapped).
:- use_module(utils/dsl).

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
    mapped(F1, folded_(F2, P, D), F, FR),
    folded_(F1, P, D, FR, R).

folded_(F1 ; F2, Pred, Zero, F, R) :-
    folded_(F1, Pred, Zero, F, R)
    ; folded_(F2, Pred, Zero, F, R).

folded_(list(_), P, D, L, R) :- foldl(P, L, D, R).

folded_(id(_), P, Z, V, R) :- call(P, Z, V, R).

folded_(dict(S, Fields), Pred, Zero, Dict, Result) :-
    is_dict(Dict, S),
    is_list(Fields),
    foldl(dict_folded_(Dict, Pred), Fields, Zero, Result).

dict_folded_(Dict, Pred, Field, Acc, NewAcc) :-
    ( integer(Field) ; atom(Field) ), !,
    get_dict(Field, Dict, Elem), 
    call(Pred, Acc, Elem, NewAcc).

dict_folded_(Dict, Pred, Field / FieldType, Acc, NewAcc) :-
    get_dict(Field, Dict, Inner), 
    folded_(FieldType, Pred, Acc, Inner, NewAcc).

