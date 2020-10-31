:- module(combined, [combined/4]).
:- use_module(library(clpfd)).
:- use_module(utils/dsl).

combined(T, A, B, C) :-
    translate(T, TT),
    combined_(TT, A, B, C).

combined_(int(+), A, B, C) :- C #= A+B.
combined_(int(*), A, B, C) :- C #= A*B.
combined_(int(min), A, B, C) :- when(( nonvar(A), nonvar(B) ), C is min(A,B)).
combined_(int(max), A, B, C) :- when(( nonvar(A), nonvar(B) ), C is max(A,B)).

combined_(list, A, B, C) :- append(A,B,C).
combined_(list / T, A, B, C) :- maplist(combined_(T), A, B, C).
combined_(id/T, A, B, C) :- combined_(T, A, B, C).

combined_(id:T, A, B, C) :- combined_(T, A, B, C).

combined_(FT:_, A, B, C) :- combined_(FT, A, B, C).

combined_(','(T1,T2), ','(A1,A2), ','(B1,B2), ','(C1,C2)) :-
    combined_(T1, A1, B1, C1),
    combined_(T2, A2, B2, C2).
