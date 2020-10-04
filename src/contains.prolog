:- module(contains, [contains/3]).

:- use_module(library(clpfd)).

:- use_module(utils/domains).
:- use_module(utils/dsl).

contains(T, A, B) :-
    translate(T, TT),
    contains_(TT, A, B).

contains_(id(_), X, X).

contains_(list(_), List, Elem) :- member(Elem, List).

contains_(elems(Domain), List, Elem) :-
    I in Domain,
    nth1(I, List, Elem).

contains_(functor(F, Arity, Fields), Func, Elem) :-
    is_list(Fields),
    member(Field, Fields),
    functor_contains(F, Arity, Field, Func, Elem).

contains_(dict(S, Fields), Dict, Elem) :-
    is_list(Fields),
    is_dict(Dict, S),
    member(Field, Fields),
    dict_contains(S, Field, Dict, Elem).

contains_(F1 / F2, C, E) :-
    contains_(F1, C, IC),
    contains_(F2, IC, E).

contains_(F1 ; F2, C, E) :-
    contains_(F1, C, E)
    ; contains_(F2, C, E).

dict_contains(S, Field / Type, Dict, Elem) :-
    is_dict(Dict, S),
    get_dict(Field, Dict, Inner),
    contains_(Type, Inner, Elem).

dict_contains(S, Field, Dict, Elem) :-
    ( integer(Field) ; atom(Field) ),
    is_dict(Dict, S),
    get_dict(Field, Dict, Elem).

functor_contains(F, Arity, Field, Func, Elem) :-
    integer(Field),
    Func =.. [F | Args],
    length(Args, Arity),
    nth1(Field, Args, Elem, _).

functor_contains(F, Arity, Field / FT, Func, Elem) :-
    functor_contains(F, Arity, Field, Func, Inner),
    contains_(FT, Inner, Elem).

