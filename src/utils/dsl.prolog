:- module(dsl, [
    translate/2
]).

:- use_module(domains).

translate(rec(R, T), IR) :-
    R = inrec(IR),
    translate(T, IR).

translate(inrec(IR), IR).

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
translate({Keys}, dict(_, NKeys)) :-
    comma_list(Keys, KeysList),
    maplist(translate_dict_field, KeysList, NKeys).

translate(functor(F, Arity, [Field|Fields]), functor(F, Arity, TRFields)) :-
    maplist(translate_functor_field, [Field|Fields], TRFields).
translate(functor(F, Arity, Field), functor(F, Arity, [TRField])) :-
    translate_functor_field(Field, TRField).
translate(at(Field), functor(_, _, [TRField])) :-
    translate_functor_field(Field, TRField).


translate_functor_field(Field, Field) :- integer(Field).
translate_functor_field(Path, Field / TRTypes) :- 
    path_to_list(Path, [Field | Types]),
    integer(Field),
    maplist(translate, Types, NTypes),
    list_to_path(NTypes, TRTypes).

translate_dict_field(Field, Field) :- integer(Field) ; atom(Field).
translate_dict_field(Field / Type, Field / TRType) :-
    (integer(Field) ; atom(Field)),
    translate(Type, TRType).

path_to_list(Path, List) :-
    phrase(path_to_list_(Path), List), !.

path_to_list_(A / B) --> path_to_list_(A), path_to_list_(B).
path_to_list_(A) --> { A \= _ / _ }, [A].

list_to_path(Types, Path) :-
    phrase(list_to_path_(Path), Types), !.

list_to_path_(A / B) --> [A], list_to_path_(B).
list_to_path_(A) --> [A].

rebalance_path(Path, NPath) :-
    path_to_list(Path, Types), 
    maplist(translate, Types, NTypes),
    list_to_path(NTypes, NPath).
