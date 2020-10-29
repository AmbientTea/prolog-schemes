:- use_module('..'/src/combined).
:- use_module(utils).

:- begin_tests('int combined tests').


test('int + test', [
    forall(( repeat(10), random_ints(X, Y), Sum is X + Y )),
    true(Result =@= Sum)
]) :-
    combined(int(+), X, Y, Result).


test('int * test', [
    forall(( repeat(10), random_ints(X, Y), Product is X * Y )),
    true(Result =@= Product)
]) :-
    combined(int(*), X, Y, Result).


test('int max test', [
    forall(( repeat(10), random_ints(X, Y), Max is max(X, Y) )),
    true(Result =@= Max)
]) :-
    combined(int(max), X, Y, Result).

test('int min test', [
    forall(( repeat(10), random_ints(X, Y), Min is min(X, Y) )),
    true(Result =@= Min)
]) :-
    combined(int(min), X, Y, Result).

:- end_tests('int combined tests').

:- begin_tests('list combined tests').

test('random lists test', [
    forall((
        repeat(10),
        random_list(List1),
        random_list(List2),
        append(List1, List2, List) 
    )),
    true(Result =@= List)
]) :-
    combined(list, List1, List2, Result).

test('empty list test - left', [
    forall((repeat(10), random_list(List))),
    true(Result =@= List)
]) :-
    combined(list:int(+), List, [], Result).

test('empty list test - right', [
    forall((repeat(10), random_list(List))),
    true(Result =@= List)
]) :-
    combined(list:int(+), [], List, Result).

:- end_tests('list combined tests').

:- begin_tests('nested list test').

test('list of ints *', [ 
    true(Result =@= [2,6,12])
]) :-
    combined(list / id(int(*)), [1,2,3], [2,3,4], Result).

:- end_tests('nested list test').

:- begin_tests('tuple combined tests').

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
    combined((int(+), int(*)), (X1, Y1), (X2, Y2), Result).

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
    combined((int(+), int(*), int(max)), (X1, Y1, Z1), (X2, Y2, Z2), Result).

:- end_tests('tuple combined tests').
