:- use_module('..'/src/mapped).
:- use_module(utils).

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

:- end_tests('mapped tests').
