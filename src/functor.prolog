:- module(functor, [map/4]).

:- meta_predicate map(?, 2, ?, ?).
map(F1 / F2, Pred, A, B) :- map(F1, map(F2, Pred), A, B).

map(list(_), Pred, A, B) :- maplist(Pred, A, B).
map(list, Pred, A, B) :- map(list(_), Pred, A, B).

map(id(_), Pred, A, B) :- call(Pred, A, B).

map(functor(F, Arity, Field), Pred, A, B) :-
    integer(Field), 
    A =.. [F | Args],
    length(Args, Arity), 
    nth1(Field, Args, Arg, Rest),
    call(Pred, Arg, NewArg),
    nth1(Field, NewArgs, NewArg, Rest),
    B =.. [F | NewArgs].

map(functor(F, Arity, Field / FT), Pred, A, B) :-
    map(functor(F, Arity, Field), map(FT, Pred), A, B).

map(functor(F, Arity, []), _, A, A) :- functor(A, F, Arity).
map(functor(F, Arity, [Field | Fields]), Pred, A, C) :-
    map(functor(F, Arity, Field), Pred, A, B),
    map(functor(F, Arity, Fields), Pred, B, C).

map(functor(Fields), Pred, A, B) :- map(functor(_, _, Fields), Pred, A, B).