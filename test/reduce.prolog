:- begin_tests('reduce tests').

:- use_module('..'/src/reduce).

test('flat list reduce', [
    forall(( 
        between(0, 10, Length), 
        randseq(Length, Length, List),
        sum_list(List, ExpectedSum)
    )),
    true(Sum =@= ExpectedSum)
]) :- 
        reduce(list(int(+)), List, Sum).

:- end_tests('reduce tests').
