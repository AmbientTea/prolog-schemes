:- module(functor, [map/4]).

:- meta_predicate map(?, 2, ?, ?).
map(F1 / F2, Pred, A, B) :- map(F1, map(F2, Pred), A, B).

map(list(_), Pred, A, B) :- maplist(Pred, A, B).
map(list, Pred, A, B) :- map(list(_), Pred, A, B).

map(functor(F, Field), Pred, A, B) :-
    integer(Field), 
    A =.. [F | Args],
    nth1(Field, Args, Arg, Rest),
    call(Pred, Arg, NewArg),
    nth1(Field, NewArgs, NewArg, Rest),
    B =.. [F | NewArgs].

map(functor(F, Field / FT), Pred, A, B) :-
    map(functor(F, Field), map(FT, Pred), A, B).

map(functor(F, []), _, A, A) :- A =.. [F | _].
map(functor(F, [Field | Fields]), Pred, A, C) :-
    map(functor(F, Field), Pred, A, B),
    map(functor(F, Fields), .Pred, B, C).

map(functor(Fields), Pred, A, B) :- map(functor(_, Fields), Pred, A, B).