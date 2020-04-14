:- module(folded, [folded/5]).

:- use_module(mapped).

:- meta_predicate folded(?, 3, ?, ?, ?).
/**
 * folded(+FoldableType, :Predicate, +Zero, +Foldable, ?Fold) is nondet
 * 
 * Folds Foldable using Predicate, with initial accumulator value Zero.
 */
folded(F1/F2, P, D, F, R) :-
    mapped(F1, folded(F2, P, D), F, FR),
    folded(F1, P, D, FR, R).

folded(list(_), P, D, L, R) :- foldl(P, L, D, R).
folded(list, P, D, L, R) :- folded(list(_), P, D, L, R).
