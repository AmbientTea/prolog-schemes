#!/usr/bin/env swipl
:- foreach(
        ( member(Dir, [test, 'test/examples']),
          directory_member(Dir, File, [matches('*.prolog')])),
        ( format("loading ~w~n", [File]), [File]) ), 
    run_tests,
    halt.

:- halt(-1).
