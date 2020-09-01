:- begin_tests('folded test').

:- use_module('..'/src/folded).


test('plain list folded', [
    forall((
        between(1, 10, Length),
        randseq(Length, Length, List),
        sum_list(List, ExpectedSum)
    )),
    true(ExpectedSum=@=Sum)
]) :-
    folded(list, plus, 0, List, Sum).


test('nested list folded', [
    forall((
        randseq(9, 10, List),
        append(List1, List2, List),
        sum_list(List, ExpectedSum)
    )),
    true(ExpectedSum=@=Sum)
]) :-
    folded(list / list(int(_)), plus, 0, [List1, List2], Sum).

test('dict folded', [ 
    setup((
        Type = dict(s, [field]),
        Dict = s{ field: 1 },
        Pred = [A, B, C]>>(C is A + B)
    )),
    true(Result =@= 4)
]) :-
    folded(Type, Pred, 3, Dict, Result).

:- end_tests('folded test').
