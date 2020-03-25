# prolog-explicit
Explicitly typed polymorphic predicate library

# Examples

## Easy data aggregations

```prolog
sum_max_min(List, Sum, Max, Min) :-
    functor:map(list, [X, (X,X,X)]>>true, List, InterList),
    reduce:reduce(list((int(+), int(max), int(min))), InterList, (Sum, Max, Min)).

:- L = [3,6,8,3,7], sum_max_min(List, Sum, Max, Min).
    Sum = 27,
    Max = 8,
    Min = 3.
```