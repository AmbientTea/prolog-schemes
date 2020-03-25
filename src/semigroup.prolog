:- module(semigroup, [combine/4]).

:- use_module(library(clpfd)).

combine(int(+), A, B, C) :- C #= A+B.
combine(int(*), A, B, C) :- C #= A*B.
combine(int(min), A, B, C) :- when(( nonvar(A), nonvar(B) ), C is min(A,B)).
combine(int(max), A, B, C) :- when(( nonvar(A), nonvar(B) ), C is max(A,B)).

combine(list(_), A, B, C) :- append(A,B,C).

combine(','(T1,T2), ','(A1,A2), ','(B1,B2), ','(C1,C2)) :-
    combine(T1, A1, B1, C1),
    combine(T2, A2, B2, C2).
