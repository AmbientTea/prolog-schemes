Easy to use optics and data structure manipulation for SWI-Prolog.

```prolog
?- BinaryTreeType = rec(Rec, (atom(nil) ; functor(node, 3, [1/Rec, 2, 3/Rec]) )),
   Tree = node(node(nil, 1, nil), 2, node(nil, 3, nil)),
   mapped(BinaryTreeType, plus(1), Tree, NewTree),
   reduced(BinaryTreeType:int(+), NewTree, Sum).
NewTree = node(node(nil, 2, nil), 3, node(nil, 4, nil)),
Sum = 9.
```

This library provides a polymorphic implementation of commonly abstracted
operations such as `map`, `fold`, `member` etc.  along with a rich DSL
for specifying complex data structures they can apply to,
in a similar fashion to optics and recursion schemes of functional languages.

# Instalation

The library is not yet packaged and the only way to install it is to
clone this repo and copy the contents to your library path.

# General structure

Every predicate exported by the library expects its first argument to be
a sufficiently grounded term describing the type that it
interprets the rest of the arguments as. Eg.:

```prolog
?- combine(int(+), 1, 2, Sum).
Sum = 3.
```

will add its arguments as integers.  
The predicates expect this type argument to be grounded only as much as needed:

```prolog
?- empty(int(Op), E).
Op = +, E = 0
; Op = *, E = 1
...
```

# Composing Types

## Content Types

Operator `:` is used to indicate the type of contents of a complex type:

```prolog
?- reduced(list:int(+), [1,2,3], Sum).
Sum = 6.
```

## Nesting

Very often data we work with is not just a simple list. 
Operator `/` composes two types together, for example: 
```prolog
?- mapped(list / list, plus(1), [[1,2],[3]], List).
List = [[2, 3], [4]].

% Sum all nested elements.
?- reduced(list/list:int(+), [[1,2],[3]], Result).
Result = 6.
```

Sometimes an operation can be nested in a couple different ways and this can be reflected
in the type: 
```prolog
% multiply the elements of inner lists and sum the results.
% notice the necessary parentheses
?- reduced((list:int(+)) / (list:int(*)), [[1,2],[3]], Result).
Result = 5 ;
```

A nice feature that comes with this syntax is that where a Scala type `List[List[Int]]`
can be ambiguously interpreted as a functor, in Prolog we can differentate between

- `list / list : int(+)` for a functor `List[List[_]]`
- `list : list : int(+)` for a functor `List[_]`.

## Alternative

Multiple possible patterns a value can take can be listed using `;` operator:
```prolog
?- reduced(list / (list ; id) : int(+), [1, [2,3], 4], Sum).
Sum = 10.
```
*Warning*: This feature can cause excessive backtracking, incorrect results or
domain errors, due to lack of reliable type information. A general rule is that
types that can be checked at runtime like lists and functors should go first.

# Supported Types

## Integers

Various ways to interpret integers are supported using the `int(Op)` type.
For now supported operations are `+`, `*`, `max` and `min`.

## Atoms

Atom literals can be specified as `atom(Atom)` and are treated as an empty functor.
```prolog
?- reduced(list / (atom(fizz) ; atom(buzz) ; id) : int(+), [1,2,fizz, 4, buzz]).
Sum = 7.
```

## Tuples

Tuple types are represented as tuples of types.
```prolog
?- empty((int(+), int(*), list), Empty).
Empty = (0, 1, []).
```

## Functors

Functor types are denoted by `functor(Symbol, Arity, Arguments)`. 
Functor `Arguments` are specified by their number (starting 1) and nested types
are supported using `/` operator:
```prolog 
?- contains(functor(f, 2, [1, 2/list]), f(1, [2]), V).
V = 1 ;
V = 2.
```
Both `Symbol` and `Arity` can be unbound.

## Dicts

SWI-Prolog's dicts are supported in a similar fashion:
```prolog 
?- contains(dict(f, [a, b/ list]), f{ a: 1, b: [2] }, V).
V = 1 ;
V = 2.
``` 

## List indexing
A subset of list indexes can be specified using CLP(FD) domain syntax:
```prolog 
?- contains(elems([1, 3..5, 7..sup]), [1,2,3,4,5,6,7,8,9], V).
V = 1 ; V = 3 ; V = 4. % etc
``` 

## Recursion
Recursion is explicitly indicated using `rec(Rec, Type)`:
```prolog
?- TreeType = rec(Rec, functor(node, 2, [1, 2/list/Rec])),
   Tree = node(1, [node(2,[]), node(3,[])]),
   mapped(TreeType, plus(1), Tree, NewTree).
NewTree = node(2, [node(3,[]), node(4,[])]).
```
Notice how the `Rec` variable is used at the recursion point.

# More Examples

## Easy data aggregation

We can make use of various `combine` operations for integers to compute simple
statistics for a given list of values:

```prolog
sum_max_min(List, Sum, Max, Min) ?-
    Type = list:(int(+), int(max), int(min)),
    mapped(Type, [X, (X,X,X)]>>true, List, InterList),
    reduced(Type, InterList, (Sum, Max, Min)).

?- List = [3,6,8,3,7], sum_max_min(List, Sum, Max, Min).
    Sum = 27,
    Max = 8,
    Min = 3.
```

Notice that since our 'types' are purely declarative, 
we can interpret the same value in terms of different algebraic structures: 
monoids with sum, max and min operation.
This does not require any boxing of the values.

## Optics and type reuse

Assume we have a list of employee contracts 
and we want to calculate the sum of the salaries.
We can take advantage of the fact that a functor can be reduced to one of its field:

```prolog
?- Employees = [
        employee(keanu, reeves, 100),
        employee(dwayne, johnson, 90),
        employee(justin, bieber, 1)
    ],
    % list of functors of arity 3, symbol `employee`, with an int in 3rd position
    EmployeeSalaries = list / functor(employee, 3, 3/int(+)),
    reduce(EmployeeSalaries, Employees, Sum).
Sum = 191.
```

Now let's say we decide to give everyone a raise:

```prolog
?- EmployeeSalaries = list / functor(employee, 3, 3),
   mapped(EmployeeSalaries, plus(10), Employees, EmployeesAfterRaise).
EmployeesAfterRaise = [employee(keanu, reeves, 110), employee(dwayne, johnson, 100), employee(justin, bieber, 11)]
```

There is not much difference between the types used in these two examples.
The only difference is that in the second one we omit the field type as there is no
`mapped` operation for ints. In cases like these we can use the `id` type to use a trivial one:

```prolog
?- Employees = [
        employee(keanu, reeves, 100),
        employee(dwayne, johnson, 90),
        employee(justin, bieber, 1)
    ],
    EmployeeSalaries = list / functor(employee, 3, 3/id(int(+))),
    reduce(EmployeeSalaries, Employees, Sum),
    mapped(EmployeeSalaries, plus(10), Employees, EmployeesAfterRaise).
Sum = 191,
EmployeesAfterRaise = [employee(keanu, reeves, 110), employee(dwayne, johnson, 100), employee(justin, bieber, 11)]
```

This is enough to make the same type work for both operations.

# Types and operations table

|            | empty | combined  | mapped  | folded | reduced | contains |
| ---------- | :---: | :-------: | :-----: | :----: | :-----: | :------: |
| composable |  no   |    no     |   yes   |  yes   |   yes   |    yes   |
| ---------  | ?---: | ?-------: | ?-----: |  ?--:  | ?----:  | :------: |
| `id`       |   x   |     x     |    x    |        |    x    |    x     |
| `int(_)`   |   x   |     x     |         |        |         |          |
| `atom(_)`  |   x   |           |    x    |        |         |    x     |
| `list`     |   x   |     x     |    x    |   x    |    x    |    x     |
| `tuple`    |   x   |     x     |         |        |         |          |
| `functor`  |   x   |           |    x    |   x    |    x    |    x     |
| `dict`     |   x   |           |    x    |   x    |    x    |    x     |
| `elems`    |       |     x     |    x    |        |         |    x     |

# DSL Shorthands

| Type    | Shorthand   |
| ------- | ----------- |
| List    | [], list    |
| Elems   | [+Domain]   |
| Dict    | {+Fields}   |
| Functor | at(+Fields) |
| Nesting | T1 / T2     |

# Mentions

This project takes a lot of inspiration from Scala projects:
[Cats](https://github.com/typelevel/cats),
[Quicklens](https://github.com/softwaremill/quicklens),
[Monocle](https://github.com/julien-truffaut/Monocle) and
[Algebird](https://github.com/twitter/algebird).
