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

test('empty dict empty create', [
    true(D =@= s{})
]) :-
    empty(dict(s), D).

test('empty dict empty check', [] ) :-
    empty(dict(s), s{}).

test('dict empty create', [
    setup((
        Fields = [a / int(*), 1 / list, b / int(+)],
        ExpectedDict = s{a: 1, 1: [], b: 0})),
    true(D =@= ExpectedDict)
]) :-
    empty(dict(s, Fields), D).

test('dict empty check', [
    setup((
        Fields = [a / int(*), 1 / list, b / int(+)],
        Expected = s{a: 1, 1: [], b: 0})),
    true(Result =@= Expected)
]) :-
    empty(dict(s, Fields), Result).

:- end_tests('empty tests').
