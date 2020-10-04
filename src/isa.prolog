:- module(isa, [isa/2]).

isa(T, _V) :- var(T).
isa(int(_), V) :- integer(V).
isa(list(_), V) :- is_list(V).
isa(dict(S, _), V) :- is_dict(V, S).
isa(functor(S, Arity, _), V) :-
    functor(V, S, Arity).
