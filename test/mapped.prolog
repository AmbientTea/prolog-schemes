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

test('dict mapped', [
    nondet,
    setup(Pred = plus(1)),
    forall((
        Type = dict(s, [field]),
        Dict = s{field: 1},
        ExpectedResult = s{field: 2}
    ) ; (
        Type = dict(s, [field1, field2]),
        Dict = s{field1: 1, field2: 2},
        ExpectedResult = s{field1: 2, field2: 3}
    ) ; (
        Type = dict(s, [field / list]),
        Dict = s{field: [1,2,3]},
        ExpectedResult = s{field: [2,3,4]}
    )
    ),
    true(Result =@= ExpectedResult)
]) :-
    mapped(Type, Pred, Dict, Result).

test('recursive tree mapped', [
    nondet,
    setup((
        Type = rec(Rec, functor(node, 2, [1, 2/list/Rec])),
        Pred = plus(10),
        Tree = node(1, [
            node(2, []),
            node(3, [
                node(4, []),
                node(5, [])
            ])
        ]),
        ExpectedResult = node(11, [
            node(12, []),
            node(13, [
                node(14, []),
                node(15, [])
            ])
        ])
    )),
    true(Result =@= ExpectedResult)
]) :-
    mapped(Type, Pred, Tree, Result).


:- end_tests('mapped tests').
