
-module(sc_list).





-export([

    between/2,

    foldl0/2,
      foldr0/2,

    max/1,
      min/1

]).





%% @doc Places a delimiter inbetween every item in the list. ```1> sc_list:between( [1,2,3], 0 ).
%% [1,0,2,0,3]
%%
%% 2> sc_list:between( [], 0 ).
%% []'''

between([],  _Delim) ->

    [];





between(List, Delim) ->

    [_|Rem] = lists:append([ [Delim, L] || L <- List ]),
    Rem.





%% @doc A traditional convenience pre-loader for `foldl', for when the initializer is the list's first member. ```1> sc_list:foldl0( fun(Step, Acc) -> Step + Acc end, [2,4,6,8] ).
%% 20
%%
%% 2> ForceList = fun(X) when is_integer(X) -> integer_to_list(X);
%% 2>                (X)                    -> X end.             
%% #Fun<erl_eval.6.106461118>
%%
%% 3> Paren = fun(X,Y) -> "(" ++ ForceList(X) ++ "," ++ ForceList(Y) ++ ")" end.
%% #Fun<erl_eval.12.106461118>
%% 
%% 4> Paren(2,3).
%% "(2,3)"
%% 
%% 5> Paren(2,"(3,4)").
%% "(2,(3,4))"
%% 
%% 6> sc_list:foldl0( Paren, [1,2,3,4,5] ).
%% "(5,(4,(3,(2,1))))"
%% 
%% 7> sc_list:foldr0( Paren, [1,2,3,4,5] ).
%% "(2,(3,(4,(5,1))))"'''

foldl0(Fun, [Head|Rem]) ->

    lists:foldl(Fun, Head, Rem).





%% @doc A traditional convenience pre-loader for `foldr', for when the initializer is the list's first member. ```1> sc_list:foldl0( fun(Step, Acc) -> Step + Acc end, [2,4,6,8] ).
%% 20
%%
%% 2> ForceList = fun(X) when is_integer(X) -> integer_to_list(X);
%% 2>                (X)                    -> X end.             
%% #Fun<erl_eval.6.106461118>
%%
%% 3> Paren = fun(X,Y) -> "(" ++ ForceList(X) ++ "," ++ ForceList(Y) ++ ")" end.
%% #Fun<erl_eval.12.106461118>
%% 
%% 4> Paren(2,3).
%% "(2,3)"
%% 
%% 5> Paren(2,"(3,4)").
%% "(2,(3,4))"
%% 
%% 6> sc_list:foldl0( Paren, [1,2,3,4,5] ).
%% "(5,(4,(3,(2,1))))"
%% 
%% 7> sc_list:foldr0( Paren, [1,2,3,4,5] ).
%% "(2,(3,(4,(5,1))))"'''

foldr0(Fun, [Head|Rem]) ->

    lists:foldr(Fun, Head, Rem).





%% @doc Returns the maximum of a non-empty list of values, type-insensitive. ```1> sc_list:max( [1, 2, 3] ).
%% 3
%%
%% 2> sc_list:max( [1, 2.0, 3] ).
%% 3
%%
%% 3> sc_list:max( [1, two, 3] ).
%% two'''

max([_|_]=List) ->

    foldl0(fun(X, Max) when X > Max -> X; (_, Max) -> Max end, List).





min([_|_]=List) ->

    foldl0(fun(X, Min) when X < Min -> X; (_, Min) -> Min end, List).
