:- use_module('..'/src/contains).

:- begin_tests('contains tests').

test('id contains itself', [
    forall(member(Term, [1, term, [list]])),
    true(Result =@= Term)
]) :-
    contains(id(int), Term, Result).

test('list contains its elements', [
    forall(member(List, [[], [1,2,3,4]])),
    all(Elem =@= List)
]) :-
    contains(list, List, Elem).

test('nested list contains its elements\' elements', [
    setup((
        LList = [[1,2,3], [4,5,6]],
        append(LList, List)
        )),
    set(Elem =@= List)
]) :-
    contains(list / list, LList, Elem).


test('single field functor', [
    setup((
        Term = f(first, second, third),
        Type = functor(F, Arity, 2)
        )),
    nondet,
    true(F - Arity - Elem =@= f - 3 - second)
]) :-
    contains(Type, Term, Elem).

test('multi-field functor', [
    setup((
        Term = f(first, second, third),
        Type = functor(_F, _Arity, [1,2])
        )),
    nondet,
    set(Elem =@= [first, second])
]) :-
    contains(Type, Term, Elem).

test('nested multi-field functor', [
    setup((
        Term = f(first, [second], third),
        Type = functor(_F, _Arity, [1/id, 2/list])
        )),
    nondet,
    set(Elem =@= [first, second])
]) :- 
    contains(Type, Term, Elem).

test('nested multi-field functor', [
    setup((
        Term = f(first, [second], third),
        Type = functor(_F, _Arity, 2/list)
        )),
    nondet,
    set(Elem =@= [second])
]) :-
    contains(Type, Term, Elem).

test('elems contains', [
    forall((
        List = [1,2,3,4,5,6,7,8,9],
        ( ( Domain = [1, 3..5], 
           Expected = [1,3,4,5]
        ) ; (
           Domain = [1..sup],
           Expected = List
        ) ; (
            Domain = [1],
            Expected = [1]
        ))
        )),
    set(Elem =@= Expected)
]) :-
    contains(elems(Domain), List, Elem).

test('dict contains', [
    forall((
        ( Type = dict(_, [a, b / list]),
          Term = symbol{ a: 1, b: [2], c: 3 },
          Expected = [1, 2])
        ; (
          Type = dict(symbol1, [a, b / list]),
          Term = symbol2{ a: 1, b: [2], c: 3 },
          Expected = [])
    )),
    set(Elem =@= Expected)
]) :-
    contains(Type, Term, Elem).

test('alternative contains', [
    setup((
        Type = list / (list ; {a}),
        Term = [[1,2], s{a: 3}],
        Expected = [1,2,3]
    )),
    set(Elem =@= Expected)
]) :-
    contains(Type, Term, Elem).

:- end_tests('contains tests').
