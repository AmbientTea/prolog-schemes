:- module(contains, [contains/3]).


contains(id(_), X, X).
contains(id, X, X).

contains(list(_), List, Elem) :- member(Elem, List).
contains(list, List, Elem) :- contains(list(_), List, Elem).

contains(functor(F, Arity, Field), Func, Elem) :-
    integer(Field),
    Func =.. [F | Args],
    length(Args, Arity),
    nth1(Field, Args, Elem, _).

contains(functor(F, Arity, Field / FT), Func, Elem) :-
    contains(functor(F, Arity, Field), Func, Inner),
    contains(FT, Inner, Elem).

contains(functor(F, Arity, [Field | Fields]), Func, Elem) :-
    contains(functor(F, Arity, Field), Func, Elem)
    ; contains(functor(F, Arity, Fields), Func, Elem).

contains(dict(S, [Field / Type]), Dict, Elem) :-
    is_dict(Dict, S),
    get_dict(Field, Dict, Inner),
    contains(Type, Inner, Elem).

contains(dict(S, [Field]), Dict, Elem) :-
    (atom(Field) ; integer(Field)),
    is_dict(Dict, S),
    get_dict(Field, Dict, Elem).

contains(F1 / F2, C, E) :-
    contains(F1, C, IC),
    contains(F2, IC, E).
