:- module(mapped, [mapped/4]).

:- use_module(library(clpfd)).

:- use_module(utils/domains).
:- use_module(utils/dsl).

:- use_module(isa).

:- meta_predicate mapped(?, 2, ?, ?).
mapped(T, Pred, A, B) :- 
    translate(T, TRT),
    mapped_(TRT, Pred, A, B).

:- discontiguous mapped_/4.
:- meta_predicate mapped_(?, 2, ?, ?).
mapped_(F1 / F2, Pred, A, B) :- mapped_(F1, mapped_(F2, Pred), A, B).

mapped_(F1 ; F2, Pred, A, B) :-
    isa(F1, A), !,
    mapped_(F1, Pred, A, B)
    ; mapped_(F2, Pred, A, B).

mapped_(list, Pred, A, B) :- maplist(Pred, A, B).

mapped_(FT:_, Pred, A, B) :- mapped_(FT, Pred, A, B).

mapped_(id(T), Pred, A, B) :- 
    isa(T, A),
    call(Pred, A, B).

mapped_(elems(Domain), Pred, A, B) :-
    map_elems(Domain, 1, Pred, A, B).

map_elems(Domain, Ind, Pred, [A | As], [B | Bs]) :-
    (Ind in Domain -> call(Pred, A, B) ; A = B),
    NInd is Ind + 1,
    map_elems(Domain, NInd, Pred, As, Bs).
map_elems(_Domain, _Ind, _Pred, [], []).

map_functor(F, Arity, Pred, Field, A, B) :-
    integer(Field), 
    A =.. [F | Args],
    length(Args, Arity), 
    nth1(Field, Args, Arg, Rest),
    call(Pred, Arg, NewArg),
    nth1(Field, NewArgs, NewArg, Rest),
    B =.. [F | NewArgs].

map_functor(F, Arity, Pred, Field / FT, A, B) :-
    map_functor(F, Arity, mapped_(FT, Pred), Field, A, B).

mapped_(functor(F, Arity, Fields), Pred, A, B) :-
    functor(A, F, Arity),
    foldl(map_functor(F, Arity, Pred), Fields, A, B).

mapped_(dict(S, [Field | Fields]), Pred, A, B) :-
    is_dict(A, S),
    maplist(mapped_dict_field(Pred, A), [Field|Fields], NewFields),
    put_dict(NewFields, A, B).

mapped_dict_field(Pred, Dict, Field / Type, Field = MElem) :-
    get_dict(Field, Dict, Elem),
    mapped_(Type, Pred, Elem, MElem).

mapped_dict_field(Pred, Dict, Field, Field = MElem) :-
    atom(Field),
    get_dict(Field, Dict, Elem),
    call(Pred, Elem, MElem).
