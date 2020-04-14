:- begin_tests('reduced tests').

:- use_module('..'/src/reduced).

test('flat list reduced', [
    forall(( 
        between(0, 10, Length), 
        randseq(Length, Length, List),
        sum_list(List, ExpectedSum)
    )),
    true(Sum =@= ExpectedSum)
]) :- 
        reduced(list(int(+)), List, Sum).

test('nested list reduced', [
    nondet,
    forall(( 
        between(0, 10, Length), 
        randseq(Length, Length, List), 
        sum_list(List, ExpectedProduct),
        append(List1, List2, List) 
    )),
    true(Product =@= ExpectedProduct)
]) :-
    reduced(list / list(int(+)), [List1, List2], Product).

test('nested list mixed reduced', [
    nondet,
    forall(( 
        between(0, 10, Length), 
        randseq(Length, Length, List), 
        append(List1, List2, List),
        sumlist(List1, Sum1),
        sumlist(List2, Sum2),
        ExpectedProduct is Sum1 * Sum2
        
    )),
    true(Product =@= ExpectedProduct)
]) :-
    reduced(list(int(*)) / list(int(+)), [List1, List2], Product).

:- end_tests('reduced tests').
