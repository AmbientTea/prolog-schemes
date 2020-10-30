:- begin_tests('reduced tests').

:- use_module('..'/src/reduced).

test('list reduced', [
    forall((
        List1 = [1,2,3],
        List2 = [4,5,6],
        append(List1, List2, List12),

        ( Type = list:int(+),
          List = [1,2,3,4,5,6],
          sum_list(List, ExpectedResult)

          ; Type = list / list : int(+),
          List = [List1, List2],
          sum_list(List12, ExpectedResult)

          ; Type = list / list / list : int(+),
          List = [[List1], [List2]],
          sum_list(List12, ExpectedResult)
        ))),
    true(Result =@= ExpectedResult)
]) :- 
    reduced(Type, List, Result).

test('nested list mixed reduced', [
    nondet,
    forall(( 
        Type = (list:int(*)) / (list:int(+)),
        List1 = [1,2,3],
        List2 = [4,5,6],
        List = [List1,List2],
        sum_list(List1, Sum1),
        sum_list(List2, Sum2),
        ExpectedProduct is Sum1 * Sum2 
    )),
    true(Product =@= ExpectedProduct)
]) :-
    reduced(Type, List, Product).

test('functor reduced', [
    nondet,
    forall((
        Type = functor(f, 3, [1]) : int(+),
        Term = f(1,a,b),
        ExpectedResult = 1

        ; Type = list / functor(f, 3, [1]) : int(+),
        Term = [f(1,a,b), f(2,c,d)],
        ExpectedResult = 3

        ; Type = functor(f, 3, [1/list, 2]) : int(+),
        Term = f([1,2,3],4,b),
        ExpectedResult = 10
    )),
    true(Result =@= ExpectedResult)
]) :-
    reduced(Type, Term, Result).

:- end_tests('reduced tests').
