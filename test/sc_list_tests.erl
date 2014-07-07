
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

    ] }.





foldl0_test_() ->

    ForceList = fun
    	(X) when is_integer(X) -> integer_to_list(X);
        (X)                    -> X 
    end,

    Paren = fun(X,Y) -> "(" ++ ForceList(X) ++ "," ++ ForceList(Y) ++ ")" end,

    { "foldl0/2", [

        { "20,  f(),     [2,4,6,8]",   ?_assert( 20                  == sc_list:foldl0( fun(Step, Acc) -> Step + Acc end, [2,4,6,8] ) ) },
        { "..., paren(), [1,2,3,4,5]", ?_assert( "(5,(4,(3,(2,1))))" == sc_list:foldl0( Paren, [1,2,3,4,5] )                          ) }

    ] }.





foldr0_test_() ->

    ForceList = fun
    	(X) when is_integer(X) -> integer_to_list(X);
        (X)                    -> X 
    end,

    Paren = fun(X,Y) -> "(" ++ ForceList(X) ++ "," ++ ForceList(Y) ++ ")" end,

    { "foldl0/2", [

        { "20,  f(),     [2,4,6,8]",   ?_assert( 20                  == sc_list:foldr0( fun(Step, Acc) -> Step + Acc end, [2,4,6,8] ) ) },
        { "..., paren(), [1,2,3,4,5]", ?_assert( "(2,(3,(4,(5,1))))" == sc_list:foldr0( Paren, [1,2,3,4,5] )                          ) }

    ] }.





max_test_() ->

    { "max/1", [

        { "[1]",         ?_assert(   1 == sc_list:max( [1] )         ) },
        { "[1, 2, 3]",   ?_assert(   3 == sc_list:max( [1, 2, 3] )   ) },
        { "[1, 2.0, 3]", ?_assert(   3 == sc_list:max( [1, 2.0, 3] ) ) },
        { "[1, two, 3]", ?_assert( two == sc_list:max( [1, two, 3] ) ) }

    ] }.
