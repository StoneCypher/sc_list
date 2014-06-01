
-module(sc_list_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").





between_test_() ->

    { "between/2", [

        { "[],            0",   ?_assert([]                    == sc_list:between([],            0)) },
        { "[1,2,3],       0",   ?_assert([1,0,2,0,3]           == sc_list:between([1,2,3],       0)) },
        { "[1,2,3],       [0]", ?_assert([1,[0],2,[0],3]       == sc_list:between([1,2,3],       [0])) },
        { "[[1],[2],[3]], [0]", ?_assert([[1],[0],[2],[0],[3]] == sc_list:between([[1],[2],[3]], [0])) }

    ]}.
