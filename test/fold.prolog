:- begin_tests('fold test').

:- use_module('..'/src/fold).


test('plain list fold', [
    forall((
        between(1, 10, Length),
        randseq(Length, Length, List),
        sum_list(List, ExpectedSum)
    )),
    true(ExpectedSum=@=Sum)
]) :-
    fold(list, plus, 0, List, Sum).


test('nested list fold', [
    forall((
        randseq(9, 10, List),
        append(List1, List2, List),
        sum_list(List, ExpectedSum)
    )),
    true(ExpectedSum=@=Sum)
]) :-
    fold(list / list(int(_)), plus, 0, [List1, List2], Sum).

:- end_tests('fold test').
