:- module(dsl, [
    translate/2
]).

translate(int(G), int(G)).
translate(int, int(_)).

translate(id, id(_)).
translate(id(T), id(T)).

translate(','(T1,T2), ','(TT1,TT2)) :-
    translate(T1, TT1),
    translate(T2, TT2).

translate(A / B, AA / BB) :-
    translate(A, AA),
    translate(B, BB).

translate(list(T), list(T)).
translate(list, list(_)).
translate([], list(_)).

translate(elems(Domain), elems(Domain)).
translate([I|Is], elems([I|Is])).
translate(elem(Domain), elems([Domain])).

translate(dict(S, [Field|Fields]), dict(S, TRFields)) :-
    maplist(translate_dict_field, [Field|Fields], TRFields).
translate(dict(S, Field), dict(S, [TRField])) :-
    translate_dict_field(Field, TRField).
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

translate_dict_field(Field, Field / id(_)) :- integer(Field) ; atom(Field).
translate_dict_field(Field / Type, Field / TRType) :-
    (integer(Field) ; atom(Field)),
    translate(Type, TRType).

