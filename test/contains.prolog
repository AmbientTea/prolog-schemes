:- use_module('..'/src/contains).

:- begin_tests('contains tests').

test('id contains itself', [
    forall(member(Term, [1, term, [list]])),
    true(Result =@= Term)
]) :-
    contains:contains(id(int), Term, Result).

test('list contains its elements', [
    forall(member(List, [[], [1,2,3,4]])),
    all(Elem =@= List)
]) :-
    contains:contains(list, List, Elem).

test('nested list contains its elements\' elements', [
    setup((
        LList = [[1,2,3], [4,5,6]],
        append(LList, List)
        )),
    set(Elem =@= List)
]) :-
    contains:contains(list / list, LList, Elem).
    

:- end_tests('contains tests').
