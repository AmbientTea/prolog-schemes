:- module(contains, [contains/3]).


contains(id(_), X, X).

contains(list(_), List, Elem) :- member(Elem, List).
contains(list, List, Elem) :- contains(list(_), List, Elem).

contains(F1 / F2, C, E) :-
    contains(F1, C, IC),
    contains(F2, IC, E).
