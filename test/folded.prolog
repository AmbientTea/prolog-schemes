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

:- end_tests('folded test').
