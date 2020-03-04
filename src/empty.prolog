 :- module(empty, [empty/2]).
 
 
empty(int(+), 0).
empty(int(*), 1).
empty(int(max), inf).
empty(int(min), -inf).

empty(list(_), []).

empty(','(T1,T2), (E1,E2)) :-
    empty(T1, E1),
    empty(T2, E2).
