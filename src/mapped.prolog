:- module(mapped, [mapped/4]).

:- use_module(library(clpfd)).

:- discontiguous mapped/4.
:- meta_predicate mapped(?, 2, ?, ?).
mapped(F1 / F2, Pred, A, B) :- mapped(F1, mapped(F2, Pred), A, B).

mapped(list(_), Pred, A, B) :- maplist(Pred, A, B).
mapped(list, Pred, A, B) :- mapped(list(_), Pred, A, B).

mapped(id(_), Pred, A, B) :- call(Pred, A, B).

mapped(elem(I), Pred, A, B) :-
    mapped(elems([I]), Pred, A, B).

mapped(elems([I | Is]), Pred, A, B) :-
    foldl([L,R,LR]>>(LR = L \/ R), Is, I, Domain),
    map_elems(Domain, 1, Pred, A, B).

map_elems(Domain, Ind, Pred, [A | As], [B | Bs]) :-
    (Ind in Domain -> call(Pred, A, B) ; A = B),
    NInd is Ind + 1,
    map_elems(Domain, NInd, Pred, As, Bs).
map_elems(_Domain, _Ind, _Pred, [], []).

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

mapped(dict(S, [Field]), Pred, A, B) :-
    (atom(Field) ; integer(Field)),
    is_dict(A, S),
    get_dict(Field, A, Elem),
    call(Pred, Elem, MElem),
    put_dict([Field = MElem], A, B).

mapped(dict(S, [Field / Type]), Pred, A, B) :-
    is_dict(A, S),
    get_dict(Field, A, Elem),
    mapped(Type, Pred, Elem, MElem),
    put_dict([Field = MElem], A, B).

