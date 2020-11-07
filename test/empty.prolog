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

dict_cases(Type, EmptyValue) :- 
    Type = dict(s),
    EmptyValue = s{}
    ;
    Fields = [a / int(*), 1 / list, b / int(+)],
    Type = dict(s, Fields),
    EmptyValue = s{a: 1, 1: [], b: 0}
    ;
    Fields = [a / int(*), 1 / list, b / int(+)],
    Type = dict(s, Fields),
    EmptyValue = s{a: 1, 1: [], b: 0}.

test('dict empty generate', [
    nondet,
    forall(dict_cases(Type, Expected)),
    true(Result =@= Expected)
] ) :-
    empty(Type, Result).


test('dict empty check', [
    nondet,
    forall(dict_cases(Type, EmptyValue))
] ) :-
    empty(Type, EmptyValue).

:- end_tests('empty tests').
