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

test('nested list reduce', [
    nondet,
    forall(( 
        between(0, 10, Length), 
        randseq(Length, Length, List), 
        sum_list(List, ExpectedProduct),
        append(List1, List2, List) 
    )),
    true(Product =@= ExpectedProduct)
]) :-
    reduce(list / list(int(+)), [List1, List2], Product).

test('nested list mixed reduce', [
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
    reduce(list(int(*)) / list(int(+)), [List1, List2], Product).

:- end_tests('reduce tests').
