:- module(utils, [
    random_ints/2,
    random_list/1,
    repeat/1
]).


random_ints(X, Y) :- 
    random_between(-1000, 1000, X),
    random_between(-1000, 1000, Y).

random_list(List) :-
        random_between(1, 1000, Len),
        randseq(Len, Len, List).

repeat(Times) :- between(1, Times, _).

