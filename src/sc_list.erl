
-module(sc_list).





-export([

    between/2

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