#!/usr/bin/env swipl
:- foreach(
        directory_member(test, File, [matches('*.prolog')]), 
        ( format("loading ~w~n", [File]), [File]) ), 
    run_tests,
    halt.

:- halt(-1).
