
-module(sc_list).





-export([

    between/2

]).





between(List, Delim) ->

    [_|Rem] = lists:append([ [Delim, L] || L <- List ]),
    Rem.