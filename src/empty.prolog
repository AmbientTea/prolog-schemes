 :- module(empty, [empty/2]).
 
 :- use_module(utils/dsl).

 empty(T, V) :-
    translate(T, TT),
    empty_(TT, V).
 
empty_(int(+), 0).
empty_(int(*), 1).
empty_(int(max), -inf).
empty_(int(min), inf).

empty_(list, []).

empty_(id/T, E) :- empty_(T, E).
empty_(id:T, E) :- empty_(T, E).

empty_(FT:_, E) :- empty_(FT, E).

empty_(','(T1,T2), (E1,E2)) :-
    empty_(T1, E1),
    empty_(T2, E2).

empty_(functor(F, Arity, Fields), E) :-
    (integer(Arity) ; var(Arity)),
    (atom(F) ; var(F)),
    length(Args, Arity), % force finite list
    E =.. [F | Args],
    maplist({Args}/[Field / FieldType]>>(
        empty_(FieldType, FE),
        nth1(Field, Args, FE)
    ), Fields).

empty_(dict(S, KeyTypes), D) :-
    sort(KeyTypes, SortedKeyTypes),
    maplist([K/T, K-E]>>empty_(T,E), SortedKeyTypes, KeyTypePairs),
    dict_pairs(D, S, KeyTypePairs).
