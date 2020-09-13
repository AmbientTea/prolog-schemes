:- module(dsl, [
    translate/2
]).

translate(A / B, AA / BB) :-
    translate(A, AA),
    translate(B, BB).

translate([], list).
translate([I|Is], elems([I|Is])).

translate({Key}, dict(_, [Key])).

translate(at(Arg), functor(_, _, [Arg])). 
