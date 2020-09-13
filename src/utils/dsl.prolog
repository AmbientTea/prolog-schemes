:- module(dsl, [
    translate/2
]).

translate(id, id(_)).
translate(id(T), id(T)).

translate(A / B, AA / BB) :-
    translate(A, AA),
    translate(B, BB).

translate(list(T), list(T)).
translate(list, list(_)).
translate([], list(_)).

translate(elems(Domain), elems(Domain)).
translate([I|Is], elems([I|Is])).
translate(elem(Domain), elems([Domain])).

translate({Key}, dict(_, [Key])).

translate(functor(F, Arity, [Field|Fields]), functor(F, Arity, TRFields)) :-
    maplist(translate_functor_field, [Field|Fields], TRFields).
translate(functor(F, Arity, Field), functor(F, Arity, [TRField])) :-
    translate_functor_field(Field, TRField).
translate(at(Field), functor(_, _, [TRField])) :-
    translate_functor_field(Field, TRField).



translate_functor_field(Field, Field / id(_)) :- integer(Field).
translate_functor_field(Field / Type, Field / TRType) :- 
    integer(Field),
    translate(Type, TRType).


