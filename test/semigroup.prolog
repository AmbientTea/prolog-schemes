:- use_module('..'/src/semigroup).

random_ints(X, Y) :- 
    random_between(-1000, 1000, X),
    random_between(-1000, 1000, Y).

random_list(List) :-
        random_between(1, 1000, Len),
        randseq(Len, Len, List).

repeat(Times) :- between(1, Times, _).

:- begin_tests('int semigroup tests').


test('int + test', [
    forall(( repeat(10), random_ints(X, Y), Sum is X + Y )),
    true(Result =@= Sum)
]) :-
    combine(int(+), X, Y, Result).


test('int * test', [
    forall(( repeat(10), random_ints(X, Y), Product is X * Y )),
    true(Result =@= Product)
]) :-
    combine(int(*), X, Y, Result).


test('int max test', [
    forall(( repeat(10), random_ints(X, Y), Max is max(X, Y) )),
    true(Result =@= Max)
]) :-
    combine(int(max), X, Y, Result).

test('int min test', [
    forall(( repeat(10), random_ints(X, Y), Min is min(X, Y) )),
    true(Result =@= Min)
]) :-
    combine(int(min), X, Y, Result).

:- end_tests('int semigroup tests').

:- begin_tests('list semigroup tests').

test('random lists test', [
    forall((
        repeat(10),
        random_list(List1),
        random_list(List2),
        append(List1, List2, List) 
    )),
    true(Result =@= List)
]) :-
    combine(list, List1, List2, Result).

test('empty list test - left', [
    forall((repeat(10), random_list(List))),
    true(Result =@= List)
]) :-
    combine(list(int(+)), List, [], Result).

test('empty list test - right', [
    forall((repeat(10), random_list(List))),
    true(Result =@= List)
]) :-
    combine(list(int(+)), [], List, Result).

:- end_tests('list semigroup tests').

:- begin_tests('tuple semigroup tests').

test('pair test', [ 
    forall((
        repeat(10),
        random_ints(X1, Y1),
        random_ints(X2, Y2),
        Sum is X1 + X2,
        Product is Y1 * Y2
        )),
    true(Result =@= (Sum, Product))
]) :-
    combine((int(+), int(*)), (X1, Y1), (X2, Y2), Result).

test('triple test', [ 
    forall((
        repeat(10),
        random_ints(X1, X2),
        random_ints(Y1, Y2),
        random_ints(Z1, Z2),
        Sum is X1 + X2,
        Product is Y1 * Y2,
        Max is max(Z1, Z2)
        )),
    true(Result =@= (Sum, Product, Max))
]) :-
    combine((int(+), int(*), int(max)), (X1, Y1, Z1), (X2, Y2, Z2), Result).

:- end_tests('tuple semigroup tests').