:- module(isa, [isa/2]).

:- use_module(utils/dsl).

isa(T, V) :- 
    translate(T, TT),
    isa_(TT, V), !.

isa_(T, _V) :- var(T), !.
isa_(T1 ; T2, V) :- 
    isa_(T1, V) ; isa_(T2, V).
isa_(id, _).
isa_(id:T, V) :- isa_(T, V).
isa_(id/T, V) :- isa_(T, V).
isa_(int(_), V) :- integer(V).
isa_(list, V) :- is_list(V).
isa_(list:_, V) :- is_list(V).
isa_(dict(S, _), V) :- is_dict(V, S).
isa_(functor(S, 0, []), S) :- atom(S).
isa_(functor(S, Arity, _), V) :-
    functor(V, S, Arity).
