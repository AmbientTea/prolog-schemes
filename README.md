# prolog-explicit
Explicitly typed polymorphic predicate library

# Rationale

Type safety is not the only benefit a type system can bring to a language.

Most functional languages leverage their type systems to create code that is more
generic and reusable. In Haskell and Scala this is achieved using type classes. 

This library attempts to translate some of these languages' goodies to Prolog,
while taking into consideration its own strengths and conventions.

This docuent talks about *type classes* and *types* in a loose way to refer to
these translations. A more correct way would probably to talk about
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
Will apply to its arguments the operation (`plus`) from the group of integers with addition.

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

# Nested Types

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

# More Examples

## Easy data aggregations

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
