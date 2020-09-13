:- module(domains, [
    sum_domains/2
]).

sum_domains([I | Is], Domain) :-
    foldl([L,R,LR]>>(LR = L \/ R), Is, I, Domain).
