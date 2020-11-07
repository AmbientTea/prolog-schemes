:- use_module('..'/'..'/src/mapped).
:- use_module('..'/'..'/src/combined).
:- use_module('..'/'..'/src/contains).

:- begin_tests('binary tree tests').

binary_tree_type(rec(Rec, (atom(nil) ; functor(node, 3, [1/Rec, 2, 3/Rec]) ))).
tree1(
    node(
        node(nil, 1, nil),
        2,
        node(
            node(nil, 3, nil),
            4,
            node(nil, 5, nil)))
).

test('mapped', [ 
    nondet,
    setup((
        binary_tree_type(Type),
        Pred = plus(10),
        tree1(Tree),
        ExpectedResult = node(
            node(nil, 11, nil),
            12,
            node(
                node(nil, 13, nil),
                14,
                node(nil, 15, nil)))
    )),
    true(Result =@= ExpectedResult)
]) :-
    mapped(Type, Pred, Tree, Result).


test('reduced', [ 
    nondet,
    setup((
        binary_tree_type(Type),
        ContentType = int(+),
        tree1(Tree),
        ExpectedResult = 15
    )),
    true(Result =@= ExpectedResult)
]) :-
    reduced:reduced(Type:ContentType, Tree, Result).


test('contains', [ 
    nondet,
    setup((
        binary_tree_type(Type),
        tree1(Tree),
        ExpectedResults = [1,2,3,4,5]
    )),
    set(Result =@= ExpectedResults)
]) :-
    contains(Type, Tree, Result).

:- end_tests('binary tree tests').
