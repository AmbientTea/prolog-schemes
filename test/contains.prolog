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


test('single field functor', [
    setup((
        Term = f(first, second, third),
        Type = functor(F, Arity, 2)
        )),
    nondet,
    true(F - Arity - Elem =@= f - 3 - second)
]) :-
    contains:contains(Type, Term, Elem).

test('multi-field functor', [
    setup((
        Term = f(first, second, third),
        Type = functor(_F, _Arity, [1,2])
        )),
    nondet,
    set(Elem =@= [first, second])
]) :-
    contains:contains(Type, Term, Elem).

test('nested multi-field functor', [
    setup((
        Term = f(first, [second], third),
        Type = functor(_F, _Arity, [1/id, 2/list])
        )),
    nondet,
    set(Elem =@= [first, second])
]) :-
    contains:contains(Type, Term, Elem).

test('nested multi-field functor', [
    setup((
        Term = f(first, [second], third),
        Type = functor(_F, _Arity, 2/list)
        )),
    nondet,
    set(Elem =@= [second])
]) :-
    contains:contains(Type, Term, Elem).

:- end_tests('contains tests').
