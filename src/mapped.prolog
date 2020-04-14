:- module(mapped, [mapped/4]).

:- meta_predicate mapped(?, 2, ?, ?).
mapped(F1 / F2, Pred, A, B) :- mapped(F1, mapped(F2, Pred), A, B).

mapped(list(_), Pred, A, B) :- maplist(Pred, A, B).
mapped(list, Pred, A, B) :- mapped(list(_), Pred, A, B).

mapped(id(_), Pred, A, B) :- call(Pred, A, B).

mapped(functor(F, Arity, Field), Pred, A, B) :-
    integer(Field), 
    A =.. [F | Args],
    length(Args, Arity), 
    nth1(Field, Args, Arg, Rest),
    call(Pred, Arg, NewArg),
    nth1(Field, NewArgs, NewArg, Rest),
    B =.. [F | NewArgs].

mapped(functor(F, Arity, Field / FT), Pred, A, B) :-
    mapped(functor(F, Arity, Field), mapped(FT, Pred), A, B).

mapped(functor(F, Arity, []), _, A, A) :- functor(A, F, Arity).
mapped(functor(F, Arity, [Field | Fields]), Pred, A, C) :-
    mapped(functor(F, Arity, Field), Pred, A, B),
    mapped(functor(F, Arity, Fields), Pred, B, C).

mapped(functor(Fields), Pred, A, B) :- mapped(functor(_, _, Fields), Pred, A, B).