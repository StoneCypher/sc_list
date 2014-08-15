
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





min_test_() ->

    { "min/1", [

        { "[1]",             ?_assert( 1 == sc_list:min( [1] )             ) },
        { "[1, 2, 3]",       ?_assert( 1 == sc_list:min( [1, 2, 3] )       ) },
        { "[1, 2.0, 3]",     ?_assert( 1 == sc_list:min( [1, 2.0, 3] )     ) },
        { "[one, 2, three]", ?_assert( 2 == sc_list:min( [one, 2, three] ) ) }

    ] }.




max_test_() ->

    { "max/1", [

        { "[1]",         ?_assert(   1 == sc_list:max( [1] )         ) },
        { "[1, 2, 3]",   ?_assert(   3 == sc_list:max( [1, 2, 3] )   ) },
        { "[1, 2.0, 3]", ?_assert(   3 == sc_list:max( [1, 2.0, 3] ) ) },
        { "[1, two, 3]", ?_assert( two == sc_list:max( [1, two, 3] ) ) }

    ] }.





prop_extrema_min_max_are_members() ->

    ?FORALL( 
    	L,
    	non_empty(list(proper_types:any())),

        case sc_list:extrema(L) of {Min,Max} -> {true,true} =:= {lists:member(Min,L),lists:member(Max,L)}; _ -> false end           
    ).


extrema_test_() ->

    { "Extrema tests", [

        {"8,6,7,5,3,0,9",                           ?_assert( {0,9}      =:= sc_list:extrema( [8,6,7,5,3,0,9] ) ) },
        {"1,2,3,4",                                 ?_assert( {1,4}      =:= sc_list:extrema( [1,2,3,4]       ) ) },
        {"-1,-2,-3",                                ?_assert( {-3,-1}    =:= sc_list:extrema( [-1,-2,-3]      ) ) },
        {"-1.1,0,1.1",                              ?_assert( {-1.1,1.1} =:= sc_list:extrema( [-1.1,1.1]      ) ) },
        {"a,b,c",                                   ?_assert( {a,c}      =:= sc_list:extrema( [a,b,c]         ) ) },
        {"1,a,{}",                                  ?_assert( {1,{}}     =:= sc_list:extrema( [1,a,{}]        ) ) },
        {"1",                                       ?_assert( {1,1}      =:= sc_list:extrema( [1]             ) ) },
        {"1,2,3,a,b,c",                             ?_assert( {1,c}      =:= sc_list:extrema( [1,2,3,a,b,c]   ) ) },

        {"[] error undefined",                      ?_assertError(function_clause, sc_list:extrema([]) ) },

        {"Stochastic: min/max are members",         ?_assert( true =:= proper:quickcheck(prop_extrema_min_max_are_members()) ) },
        {"Stochastic error: not a list type error", ?_assertError(function_clause, sc_list:extrema([]) ) }

    ] }.





prop_key_duplicate_correct_length() ->

    ?FORALL( { L,                              I                  },
             { proper_types:non_neg_integer(), proper_types:any() },

             abs(L) == length( sc_list:key_duplicate([ {abs(L),I} ]) )

           ).


key_duplicate_test_() ->

    { "Key duplicate tests", [

        {"[ ]",                                                   ?_assert( []                                                           =:= sc_list:key_duplicate([ ])                                                ) },
        {"[ {2,a} ]",                                             ?_assert( [a,a]                                                        =:= sc_list:key_duplicate([ {2,a} ])                                          ) },
        {"[ {2,a},{3,b} ]",                                       ?_assert( [a,a,b,b,b]                                                  =:= sc_list:key_duplicate([ {2,a},{3,b} ])                                    ) },
        {"[ {3,bork} ]",                                          ?_assert( [bork,bork,bork]                                             =:= sc_list:key_duplicate([ {3,bork} ])                                       ) },
        {"[ {3,sunday}, {2,monster}, {2,truck}, {1,'MADNESS'} ]", ?_assert( [sunday,sunday,sunday,monster,monster,truck,truck,'MADNESS'] =:= sc_list:key_duplicate([ {3,sunday},{2,monster},{2,truck},{1,'MADNESS'} ]) ) },

        {"Regression: key empty-list",                            ?_assert( [ [], [] ]  =:= sc_list:key_duplicate([ {2, []} ])     ) },

        {"Spec test",                                             ?_assert( true =:= proper:check_spec({sc_list,key_duplicate,1}) ) },

        {"Stochastic: correct length",                            ?_assert( true =:= proper:quickcheck(prop_key_duplicate_correct_length()) ) }

    ] }.





implode_test_() ->

    { "Implode tests", [

        {"a,b,c",    ?_assert("a,b,c"    =:= sc_list:implode(",", ["a",  "b",  "c" ]))},
        {"ab,cd,ef", ?_assert("ab,cd,ef" =:= sc_list:implode(",", ["ab", "cd", "ef"]))},
        {",,",       ?_assert(",,"       =:= sc_list:implode(",", ["",   "",   ""  ]))},
        {"",         ?_assert(""         =:= sc_list:implode("",  ["",   "",   ""  ]))}

    ] }.





prop_rotate_list_correct_length() ->

    ?FORALL( { R,     L                        },
             { int(), list(proper_types:any()) },

             length(L) == length(sc_list:rotate_list(R,L))

           ).





prop_rotate_list_same_histo() ->

    ?FORALL( { R,     L                        },
             { int(), list(proper_types:any()) },

             sc_list:histograph(L) == sc_list:histograph(sc_list:rotate_list(R,L))

           ).





rotate_list_test_() ->

    { "Rotate list tests", [

        {"0,  [ ]",       ?_assert( []      =:= sc_list:rotate_list(0,  [ ])       ) },
        {"1,  [ ]",       ?_assert( []      =:= sc_list:rotate_list(1,  [ ])       ) },
        {"-1, [ ]",       ?_assert( []      =:= sc_list:rotate_list(-1, [ ])       ) },

        {"0,  [ a,b,c ]", ?_assert( [a,b,c] =:= sc_list:rotate_list(0,  [ a,b,c ]) ) },
        {"1,  [ a,b,c ]", ?_assert( [b,c,a] =:= sc_list:rotate_list(1,  [ a,b,c ]) ) },
        {"-1, [ a,b,c ]", ?_assert( [c,a,b] =:= sc_list:rotate_list(-1, [ a,b,c ]) ) },
        {"3,  [ a,b,c ]", ?_assert( [a,b,c] =:= sc_list:rotate_list(3,  [ a,b,c ]) ) },
        {"-3, [ a,b,c ]", ?_assert( [a,b,c] =:= sc_list:rotate_list(-3, [ a,b,c ]) ) },
        {"9,  [ a,b,c ]", ?_assert( [a,b,c] =:= sc_list:rotate_list(9,  [ a,b,c ]) ) },

        {"Stochastic: correct length",  ?_assert( true  =:= proper:quickcheck(prop_rotate_list_correct_length()) ) },
        {"Stochastic: same histograph", ?_assert( true  =:= proper:quickcheck(prop_rotate_list_same_histo()) ) }

    ] }.





list_intersection_test_() ->

    { "List intersection tests", [

        {"[3,1,4],[1,5,9]", ?_assert( [1]     =:= sc_list:intersection([3,1,4],[1,5,9]) )},
        {"[3,1,4],[2,5,9]", ?_assert( []      =:= sc_list:intersection([3,1,4],[2,5,9]) )},
        {"[3,1,4],[1,4,3]", ?_assert( [4,3,1] =:= sc_list:intersection([3,1,4],[1,4,3]) )},
        {"[3,1,4],[3,1,4]", ?_assert( [4,3,1] =:= sc_list:intersection([3,1,4],[3,1,4]) )},
        {"[3,a,4],[a,5,3]", ?_assert( [a,3]   =:= sc_list:intersection([3,a,4],[a,5,3]) )}

    ] }.
