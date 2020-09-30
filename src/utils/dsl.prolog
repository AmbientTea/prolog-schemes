:- module(dsl, [
    translate/2
]).

:- use_module(domains).

translate(int(G), int(G)).
translate(int, int(_)).

translate(id, id(_)).
translate(id(T), id(T)).

translate(','(T1,T2), ','(TT1,TT2)) :-
    translate(T1, TT1),
    translate(T2, TT2).

translate(A / B, NPath) :-
    rebalance_path(A/B, NPath).

translate(list(T), list(T)).
translate(list, list(_)).
translate([], list(_)).

translate(elems(Domains), elems(Domain)) :-
    is_list(Domains) -> sum_domains(Domains, Domain) ; Domain = Domains.
translate([I|Is], elems(Domain)) :-
    sum_domains([I|Is], Domain).
translate(elem(Domain), elems(Domain)).

translate(dict(S, [Field|Fields]), dict(S, TRFields)) :-
    maplist(translate_dict_field, [Field|Fields], TRFields).
translate(dict(S, Field), dict(S, [TRField])) :-
    translate_dict_field(Field, TRField).
translate(dict(S), dict(S, [])).
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

unfoldl(Pred, [H|T], V0, V) :-
    call(Pred, H, V0, V1),
    unfoldl(Pred, T, V1, V).
unfoldl(_, [], V, V).

slash_r(A, B, A/B).
slash_l(A, B/A, B).

rebalance_path(Path, NPath) :-
    unfoldl(slash_l, Types, Path, X), !,
    maplist(translate, [X | Types], [TX|TTypes]),
    append([TTypes, [TX]], [TT|TTypes2]),
    foldl(slash_r, TTypes2, TT, NPath).
