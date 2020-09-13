:- module(mapped, [mapped/4]).

:- use_module(library(clpfd)).

:- use_module(utils/domains).
:- use_module(utils/dsl).

:- meta_predicate mapped(?, 2, ?, ?).
mapped(T, Pred, A, B) :- 
    translate(T, TRT),
    mapped_(TRT, Pred, A, B).

:- discontiguous mapped_/4.
:- meta_predicate mapped_(?, 2, ?, ?).
mapped_(F1 / F2, Pred, A, B) :- mapped_(F1, mapped_(F2, Pred), A, B).

mapped_(list(_), Pred, A, B) :- maplist(Pred, A, B).

mapped_(id(_), Pred, A, B) :- call(Pred, A, B).

mapped_(elems([I | Is]), Pred, A, B) :-
    sum_domains([I|Is], Domain),
    map_elems(Domain, 1, Pred, A, B).

map_elems(Domain, Ind, Pred, [A | As], [B | Bs]) :-
    (Ind in Domain -> call(Pred, A, B) ; A = B),
    NInd is Ind + 1,
    map_elems(Domain, NInd, Pred, As, Bs).
map_elems(_Domain, _Ind, _Pred, [], []).

mapped_(functor(F, Arity, Field), Pred, A, B) :-
    integer(Field), 
    A =.. [F | Args],
    length(Args, Arity), 
    nth1(Field, Args, Arg, Rest),
    call(Pred, Arg, NewArg),
    nth1(Field, NewArgs, NewArg, Rest),
    B =.. [F | NewArgs].

mapped_(functor(F, Arity, Field / FT), Pred, A, B) :-
    mapped_(functor(F, Arity, Field), mapped_(FT, Pred), A, B).

mapped_(functor(F, Arity, []), _, A, A) :- functor(A, F, Arity).
mapped_(functor(F, Arity, [Field | Fields]), Pred, A, C) :-
    mapped_(functor(F, Arity, Field), Pred, A, B),
    mapped_(functor(F, Arity, Fields), Pred, B, C).

mapped_(dict(S, [Field / Type]), Pred, A, B) :-
    is_dict(A, S),
    get_dict(Field, A, Elem),
    mapped_(Type, Pred, Elem, MElem),
    put_dict([Field = MElem], A, B).

