:- module(combined, [combined/4]).
% contains(functor(_50422,_50424,[1/id,2/list]),f(first,[second],third),_50470)
:- use_module(library(clpfd)).

combined(int(+), A, B, C) :- C #= A+B.
combined(int(*), A, B, C) :- C #= A*B.
combined(int(min), A, B, C) :- when(( nonvar(A), nonvar(B) ), C is min(A,B)).
combined(int(max), A, B, C) :- when(( nonvar(A), nonvar(B) ), C is max(A,B)).

combined(list(_), A, B, C) :- append(A,B,C).
combined(list, A, B, C) :- combined(list(_), A, B, C).
combined(list / T, A, B, C) :- maplist(combined(T), A, B, C).

combined(id(T), A, B, C) :- combined(T, A, B, C).

combined(','(T1,T2), ','(A1,A2), ','(B1,B2), ','(C1,C2)) :-
    combined(T1, A1, B1, C1),
    combined(T2, A2, B2, C2).
