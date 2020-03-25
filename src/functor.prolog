:- module(functor, [map/4]).

:- meta_predicate map(?, 2, ?, ?).
map(F1 / F2, Pred, A, B) :- map(F1, map(F2, Pred), A, B).

map(list(_), Pred, A, B) :- maplist(Pred, A, B).
map(list, Pred, A, B) :- map(list(_), Pred, A, B).

map(functor(Field), Pred, A, B) :-
    integer(Field), 
    A =.. [F | Args],
    nth1(Field, Args, Arg, Rest),
    call(Pred, Arg, NewArg),
    nth1(Field, NewArgs, NewArg, Rest),
    B =.. [F | NewArgs].
