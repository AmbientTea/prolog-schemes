:- use_module('..'/src/empty).

:- begin_tests('empty tests').

test('single-field functor', [
    setup((
        ExpectedTerm = f(1, _, _),
        Type = functor(f, 3, 1 / int(*))
        )),
    nondet,
    true(Term =@= ExpectedTerm)
]) :-
    empty(Type, Term).

test('multi-field functor', [
    setup((
        ExpectedTerm = f(1, _, 0),
        Type = functor(f, 3, [ 1 / int(*), 3 / int(+) ])
        )),
    nondet,
    true(Term =@= ExpectedTerm)
]) :-
    empty(Type, Term).

:- end_tests('empty tests').
