 :- module(empty, [empty/2]).
 
 
empty(int(+), 0).
empty(int(*), 1).
empty(int(max), -inf).
empty(int(min), inf).

empty(list(_), []).
empty(list, []).

empty(id(T), E) :- empty(T, E).

empty(','(T1,T2), (E1,E2)) :-
    empty(T1, E1),
    empty(T2, E2).

empty(functor(F, Arity, Field / FT), E) :-
    integer(Field),
    empty(FT, FE),
    nth1(Field, Args, FE),
    length(Args, Arity), % force finite list
    E =.. [F | Args].