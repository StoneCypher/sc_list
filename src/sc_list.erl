
-module(sc_list).





-export([

    between/2,

    foldl0/2,
      foldr0/2,

    extrema/1,
      min/1,
      max/1,

    key_duplicate/1,

    implode/2

]).





%% @doc Places a delimiter inbetween every item in the list. ```1> sc_list:between( [1,2,3], 0 ).
%% [1,0,2,0,3]
%%
%% 2> sc_list:between( [], 0 ).
%% []'''
%%
%% Unit and doc tested.

-spec between(List::list(), Delim::any()) -> list().

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
%%
%% Unit and doc tested.
%%
%% @todo Stochastic match test of foldr0/2 to foldr/3

-spec foldl0(Fun::fun(), List::nonempty_list()) -> any().

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
%%
%% Unit and doc tested.
%%
%% @todo Stochastic match test of foldr0/2 to foldr/3

-spec foldr0(Fun::fun(), List::nonempty_list()) -> any().

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
%%
%% Unit and doc tested.

-spec max(Name::nonempty_list()) -> any().

max([_|_]=List) ->

    foldl0(fun(X, Max) when X > Max -> X; (_, Max) -> Max end, List).





%% @doc Returns the minimum of a non-empty list of values, type-insensitive. ```1> sc_list:min( [1, 2, 3] ).
%% 1
%%
%% 2> sc_list:min( [1, 2.0, 3] ).
%% 1
%%
%% 3> sc_list:min( [one, 2, three] ).
%% 2'''
%%
%% Unit and doc tested.

-spec min(Name::nonempty_list()) -> any().

min([_|_]=List) ->

    foldl0(fun(X, Min) when X < Min -> X; (_, Min) -> Min end, List).





%% @doc <span style="color: green; font-weight: bold;">Stoch</span> Returns the lowest and highest values in a list of one or more member in the form `{Lo,Hi}'.  Undefined over the empty list.  Mixed-type safe; sorts according to type order rules.  ```1> sc_list:extrema([1,2,3,4]).
%% {1,4}
%%
%% 2> sc_list:extrema([1,2,3,a,b,c]).
%% {1,c}'''
%%
%% 3> sc_list:extrema( [] ).
%% ** exception error: no function clause matching sc_list:extrema([])'''
%%
%% Unit, doc and stochastic (min and max are list members) tested.

-spec extrema(List::nonempty_list()) -> { Low::any(), Hi::any() }.

extrema([First | _] = List) ->

    Next = fun(Next,T) ->

        {Lo, Hi} = T,

        Lo2 = if
            Next < Lo -> Next;
            true      -> Lo
        end,

        Hi2 = if
            Next > Hi -> Next;
            true      -> Hi
        end,

        {Lo2, Hi2}

    end,

    lists:foldl(Next, {First,First}, List).





%% @doc <span style="color: green; font-weight: bold;">Stoch, spec</span> Iterates a list of `{Count,Term}', producing a list of `[Term,Term,...]'.  ```1> sc_list:key_duplicate([ {3,bork} ]).
%% [bork,bork,bork]
%%
%% 2> sc_list:key_duplicate([ {3,sunday}, {2,monster}, {2,truck}, {1,'MADNESS'} ]).
%% [sunday,sunday,sunday,monster,monster,truck,truck,'MADNESS']'''
%%
%% Unit, doc, spec and stochastic (correct length) tested.

-spec key_duplicate(KeyList::list({non_neg_integer(),any()})) -> [any()].

key_duplicate(KeyList) ->

    lists:append( [ lists:duplicate(Key, Value) || {Key,Value} <- KeyList ] ).





%% @since Version 621
%%
%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Append strings with separating string inbetween - contrast {@link explode/2}. ```1> sc_list:implode(",", ["a", "b", "c"]).
%% "a,b,c"
%%
%% 2> sc_list:implode(",", ["ab", "cd", "ef"]).
%% "ab,cd,ef"
%%
%% 3> sc_list:implode(",", ["", "", ""]).
%% ",,"
%%
%% 4> sc_list:implode("-wop ", ["do", "do", "do"]).
%% "do-wop do-wop do"
%%
%% 5> sc_list:implode("", ["", "", ""]).
%% []'''
%%
%% thanks for a much better implementation, etnt

implode(Separator, Data)

    when is_list(Data),
         is_list(Separator) ->

    lists:append(
        lists:foldr(

            fun(Item, [])  -> [Item];
               (Item, Acc) -> [Item] ++ [Separator] ++ Acc
            end,

            "",
            Data

        )
    ).
