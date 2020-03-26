# prolog-explicit
Explicitly typed polymorphic predicate library

# Rationale

Type safety is not the only benefit a type system can bring to a language.

Most functional languages leverage their type systems to create code that is more
generic and reusable. In Haskell and Scala this is achieved using type classes. 

This library attempts to translate some of these languages' goodies to Prolog,
while taking into consideration its own strengths and conventions.

This docuent still talks about *type classes* and *types* in a loose way to refer to
these translations even in absence of a real type system. 
A more correct way would probably to talk about
*operations* (`map` as opposed to `Functor`)
and *algebraic structures* (`(Int, +) group` as opposed to `Int`).

# Instalation

The library is not yet packaged and the only way to install it is to
clone this repo and copy the contents to your library path.

# General structure
Every predicate exported by the library expects its first argument to be
a sufficiently grounded term describing the algebraic structure that it
should use to interpret the rest of the arguments. Eg.
```prolog
:- combine(int(+), 1, 2, Sum).
Sum = 3.
```
Will apply to its arguments the operation (`plus`) of the group of `integers with addition`.

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

Someties an operation can be nested in a couple different ways and this can be reflected
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

## Tuples

Some operations can be derived for tuples based on the operations of their contents:
```prolog 
:- empty((int(+), int(*), list), Empty).
Empty = (0, 1, []).
```

# More Examples

## Easy data aggregation
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

## Optics and type reuse
Assume we have a database of employee contracts and we want to calculate
sthe sum of the salaries.
We can take advantage of the fact that a functor can be reduced to one of its field:
```prolog
:- Employees = [
        employee(keanu, reeves, 100),
        employee(dwayne, johnson, 90),
        employee(justin, bieber, 1)
    ],
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
functor instance for ints. In cases like these we can use the `id` functor to make an
arbitrary type behave like one:
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