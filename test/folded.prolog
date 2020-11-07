:- begin_tests('folded test').

:- use_module('..'/src/folded).


test('list folded', [
    forall((
        Type = list,
        List = [1,2,3,4,5,6],
        sum_list(List, ExpectedResult)

      ; Type = list / list : int(_),
        List1 = [1,2,3],
        List2 = [4,5,6],
        List = [List1, List2],
        append(List1, List2, List12),
        sum_list(List12, ExpectedResult)
    )),
    true(ExpectedResult=@=Result)
]) :-
    folded(Type, plus, 0, List, Result).

test('dict folded', [ 
    forall((
        Type = dict(s, [field]),
        Dict = s{ field: 1 },
        Pred = plus,
        Zero = 3,
        ExpectedResult = 4

      ; Type = dict(s, [field / list]),
        Dict = s{ field: [1,2,3,4,5] },
        Pred = plus,
        Zero = 0,
        ExpectedResult = 15
    )),
    true(Result =@= ExpectedResult)
]) :-
    folded(Type, Pred, Zero, Dict, Result).

test('alternative folded', [
    nondet,
    setup((
        Type = list / (list ; id) / id : int,
        Pred = plus,
        Term = [1, [2,3], 4],
        ExpectedSum = 10
    )),
    true(Result =@= ExpectedSum)
]) :-
    folded(Type, Pred, 0, Term, Result).

:- end_tests('folded test').
