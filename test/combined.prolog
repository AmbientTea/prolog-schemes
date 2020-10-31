:- use_module('..'/src/combined).
:- use_module(utils).

:- begin_tests('combined tests').

test('int combined tests', [
    forall((
        repeat(10),
        random_ints(X, Y),
        ( Type = int(+), ExpectedResult is X + Y 
        ; Type = int(*), ExpectedResult is X * Y 
        ; Type = int(min), ExpectedResult is min(X,Y)
        ; Type = int(max), ExpectedResult is max(X,Y) )
    )),
    true(Result =@= ExpectedResult)
]) :-
    combined(Type, X, Y, Result).

test('lists combined tests', [
    forall((
        List1 = [1,2,3],
        List2 = [4,5,6],
        append(List1, List2, List12),
        ( Type = list,
          A = List1, B = List2,
          ExpectedResult = List12

        ; Type = list : int(+),
          A = List1, B = List2,
          ExpectedResult = List12

        ; Type = list / id / int(+),
          A = List1, B = List2,
          ExpectedResult = [5, 7, 9]
        )
    )),
    true(Result =@= ExpectedResult)
]) :-
    combined(Type, A, B, Result).

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

:- end_tests('combined tests').
