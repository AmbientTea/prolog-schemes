:- module(contains, [contains/3]).

:- use_module(library(clpfd)).

:- use_module(utils/domains).
:- use_module(utils/dsl).

contains(T, A, B) :-
    translate(T, TT),
    contains_(TT, A, B).

contains_(id(_), X, X).

contains_(list(_), List, Elem) :- member(Elem, List).

contains_(elems(Domains), List, Elem) :-
    sum_domains(Domains, Domain), 
    I in Domain,
    nth1(I, List, Elem).

contains_(functor(F, Arity, Field), Func, Elem) :-
    integer(Field),
    Func =.. [F | Args],
    length(Args, Arity),
    nth1(Field, Args, Elem, _).

contains_(functor(F, Arity, Field / FT), Func, Elem) :-
    contains_(functor(F, Arity, Field), Func, Inner),
    contains_(FT, Inner, Elem).

contains_(functor(F, Arity, [Field | Fields]), Func, Elem) :-
    contains_(functor(F, Arity, Field), Func, Elem)
    ; contains_(functor(F, Arity, Fields), Func, Elem).

contains_(dict(S, [Field / Type]), Dict, Elem) :-
    is_dict(Dict, S),
    get_dict(Field, Dict, Inner),
    contains_(Type, Inner, Elem).

contains_(dict(S, [Field]), Dict, Elem) :-
    (atom(Field) ; integer(Field)),
    is_dict(Dict, S),
    get_dict(Field, Dict, Elem).

contains_(F1 / F2, C, E) :-
    contains_(F1, C, IC),
    contains_(F2, IC, E).
