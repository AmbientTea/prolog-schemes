# Rationale

Type safety is not the only benefit a type system can bring to a language.

Most functional languages leverage their type systems to create code that is more
generic and reusable. In Haskell and Scala this is achieved using type classes.

This library attempts to translate some of these languages' goodies to Prolog,
while taking into consideration its own strengths and conventions.

This document still talks about _type classes_ and _types_ in a loose way
to refer to these constructs even in absence of a real type system.
A more correct way would probably to talk about
_operations_ (`map` as opposed to `Functor`)
and _algebraic structures_ (`(Int, +) group` as opposed to `Int`).

# Instalation

The library is not yet packaged and the only way to install it is to
clone this repo and copy the contents to your library path.

# General structure

Every predicate exported by the library expects its first argument to be
a sufficiently grounded term describing the algebraic structure that it
should use to interpret the rest of the arguments. Eg.:

```prolog
:- combine(int(+), 1, 2, Sum).
Sum = 3.
```

will apply to its arguments the operation (`plus`) of the group of `integers with addition`.

The predicates expect this type argument to be grounded only as much as needed:

```prolog
:- combine(list(_), [1], [2], List).
List = [1, 2]
```

Some shorthands are also provided:

```prolog
:- combine(list, [1], [2], List).
List = [1, 2]
```

# Complex Types

## Nesting

Very often data we work with is not just a simple list. That is why the library provides
the operator `/` for composing multiple instances of the same 'type class'. For example:

```prolog
:- functor:map(list / list(_), plus(1), [[1,2],[3]], List).
List = [[2, 3], [4]].
```

Sometimes an operation can be nested in a couple different ways and this can be reflected
in the type:

```prolog
% multiply the elements of inner lists and sum the results.
:- reduce:reduce(list(int(+)) / list(int(*)), [[1,2],[3]], Result).
Result = 5 ;
```

Shorthands are allowed in special cases:

```prolog
% Sum all nested elements.
:- reduce:reduce(list / list(int(+)), [[1,2],[3]], Result).
Result = 6 ;
```

A nice feature that comes with this syntax is that where a Scala type `List[List[Int]]`
can be ambiguously interpreted as a functor, in Prolog we can differentate between
* `list / list(int(+))` for a functor `List[List[_]]`
* `list(list(int(+)))` for a functor `List[_]`.

## Tuples

Some operations can be derived for tuples based on the operations of their contents:

```prolog
:- empty((int(+), int(*), list), Empty).
Empty = (0, 1, []).
```

# More Examples

## Easy data aggregation

We can make use of various `combine` operations for integers to compute simple
statistics for a given list of values:
```prolog
sum_max_min(List, Sum, Max, Min) :-
    Type = list((int(+), int(max), int(min)))
    functor:map(Type, [X, (X,X,X)]>>true, List, InterList),
    reduce:reduce(Type, InterList, (Sum, Max, Min)).

:- List = [3,6,8,3,7], sum_max_min(List, Sum, Max, Min).
    Sum = 27,
    Max = 8,
    Min = 3.
```
Notice that since our 'types' are purely declarative, we can interpret the same value in terms
of different algebraic structures: groups with sum, max and min. This does not require any
boxing of the values, like the Twitter's algebird
[Min and Max](https://twitter.github.io/algebird/datatypes/min_and_max.html) do.

## Optics and type reuse

Assume we have a database of employee contracts and we want to calculate
the sum of the salaries.
We can take advantage of the fact that a functor can be reduced to one of its field:

```prolog
:- Employees = [
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
:- EmployeeSalaries = list / functor(employee, 3, 3),
   map(EmployeeSalaries, plus(10), Employees, EmployeesAfterRaise).
EmployeesAfterRaise = [employee(keanu, reeves, 110), employee(dwayne, johnson, 100), employee(justin, bieber, 11)]
```

It's easy to see there is not much difference between the types used in these two examples.
The only difference is that in the second one we omit the field type as there is no
`map` operation for ints. In cases like these we can use the `id` type to use a trivial one:

```prolog
:- Employees = [
        employee(keanu, reeves, 100),
        employee(dwayne, johnson, 90),
        employee(justin, bieber, 1)
    ],
    EmployeeSalaries = list / functor(employee, 3, 3/id(int(+))),
    reduce(EmployeeSalaries, Employees, Sum),
    map(EmployeeSalaries, plus(10), Employees, EmployeesAfterRaise).
Sum = 191,
EmployeesAfterRaise = [employee(keanu, reeves, 110), employee(dwayne, johnson, 100), employee(justin, bieber, 11)]
```

This is enough to make the same type work for both operations.

# Instance table

|            | empty | semigroup | functor | fold | reduce |
| ---------- | :---: | :-------: | :-----: | :--: | :----: |
| composable |  no   |    no     |   yes   | yes  |  yes   |
| ---------  | :---: | :-------: | :-----: | :--: | :----: |
| `id(_)`    |   x   |     x     |    x    |      |   x    |
| `int(_)`   |   x   |     x     |         |      |        |
| `list(_)`  |   x   |     x     |    x    |  x   |   x    |
| `tuple`    |   x   |     x     |         |      |        |
| `functor`  |  `*`  |           |    x    |      |  `**`  |

`*` Arity 1 only

`**` Arity 1 only

# Mentions

This project takes a lot of inspiration from Scala projects:
[Cats](https://github.com/typelevel/cats),
[Monocle](https://github.com/julien-truffaut/Monocle) and
[Algebird](https://github.com/twitter/algebird).