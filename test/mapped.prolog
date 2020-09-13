:- use_module('..'/src/mapped).
:- use_module(utils).
:- use_module(library(clpfd)).

:- begin_tests('mapped tests').

test('list mapped', [
    forall((
        repeat(10),
        random_list(List), 
        member(Pred, [plus(1), [X,Y]>>(Y=[X])]),
        maplist(Pred, List, ExpectedResult)
    )),
    true(Result =@= ExpectedResult)
]) :-
    mapped(list, Pred, List, Result).

test('functor mapped', [
    nondet,
    forall((
        Type = functor(f, 3, 2),
        Pred = plus(1),
        F = f(1,2,4),
        Expected = f(1,3,4)
    ) ; (
        Type = functor(f, 3, 2/list),
        Pred = plus(1),
        F = f(1,[1,2,3],4),
        Expected = f(1,[2,3,4],4)
    ) ; (
        Type = functor(f, 3, [2, 3, 1/list]),
        Pred = plus(1),
        F = f([1,2,3],2,4),
        Expected = f([2,3,4],3,5)
    )),
    true(Result =@= Expected)
]) :- 
    mapped(Type, Pred, F, Result).

test('elems mapped', [
    nondet,
    setup(Pred = plus(1)),
    forall(( 
        Type = elem(1..3),
        List = [1,2,3,4,5,6],
        ExpectedResult = [2,3,4,4,5,6]
    ) ; (
        Type = elems([1,3,5]),
        List = [1,2,3,4,5,6],
        ExpectedResult = [2,2,4,4,6,6]
    ) ; (
        Type = elems([1,4..sup]),
        List = [1,2,3,4,5,6],
        ExpectedResult = [2,2,3,5,6,7]
    )
    ),
    true(Result =@= ExpectedResult)
]) :-
    mapped(Type, Pred, List, Result).

:- end_tests('mapped tests').
