
-module(sc_list).





-export([

    between/2,

    foldl0/2,
      foldr0/2,

    extrema/1,
      min/1,
      max/1,

    key_duplicate/1,

    implode/2,

    rotate_list/2,

    histograph/1,

    intersection/2,
      intersection/3


]).





-type weighted_value(T)          :: { Value::T, Weight::number() }.        %% Used by functions like weighted_arithmetic_mean/1 and from_weighted/1, weighted_value()s represent a value with an associated importance or "weight".
-type weight_list(T)             :: [ weighted_value(T) ].                 %% All members of weightlists must be weighted_value(T)s.





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






%% @doc <span style="color: green; font-weight: bold;">Tested</span> Rotates the front `Distance' elements of a list to the back, in order.  Negative distances rotate the back towards the front.  Distances over the length of
%% the list wrap in modulus.  ```1> sc_list:rotate_list(2, [1,2,3,4,5,6,7,8]).
%% [3,4,5,6,7,8,1,2]
%%
%% 2> sc_list:rotate_list(-2, [1,2,3,4,5,6,7,8]).
%% [7,8,1,2,3,4,5,6]
%%
%% 3> sc_list:rotate_list(0, [1,2,3,4,5,6,7,8]).
%% [1,2,3,4,5,6,7,8]
%%
%% 4> sc_list:rotate_list(16, [1,2,3,4,5,6,7,8]).
%% [1,2,3,4,5,6,7,8]'''

-spec rotate_list(Distance::integer(), ListData::list()) -> list().

rotate_list(_, []) ->

    [];



rotate_list(0, List) ->

    List;



rotate_list(By, List)

    when By =< (-(length(List))) ->

    rotate_list(By rem length(List), List);



rotate_list(By, List)

    when By < 0 ->

    rotate_list(length(List) + By, List);



rotate_list(By, List)

    when By >= length(List) ->

    rotate_list(By rem length(List), List);



rotate_list(By, List) ->

    { Front, Rear } = lists:split(By, List),

    Rear ++ Front.




%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes a histograph count of the items in the list.  Mixed type lists are safe.  Input lists do not need to be sorted.  The histograph is shallow - that is, the histograph of `[ [1,2], [1,2], [2,2] ]' is `[ {[1,2],2}, {[2,2],1} ]', not `[ {1,2}, {2,4} ]'. ```1> sc_list:histograph([1,2,a,2,b,1,b,1,b,2,a,2,2,1]).
%% [{1,4},{2,5},{a,2},{b,3}]
%%
%% 2> sc_list:histograph([ sc:rand(10) || X <- lists:seq(1,100000) ]).
%% [{0,10044}, {1,9892}, {2,10009}, {3,10016}, {4,10050}, {5,10113}, {6,9990}, {7,9994}, {8,10004}, {9,9888}]
%%
%% 3> ChessBoard = [ rook,  knight, bishop, king,  queen, bishop, knight, rook,
%%                   pawn,  pawn,   pawn,   pawn,  pawn,  pawn,   pawn,   pawn,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   pawn,  pawn,   pawn,   pawn,  pawn,  pawn,   pawn,   pawn,
%%                   rook,  knight, bishop, king,  queen, bishop, knight, rook ].
%% [rook,knight,bishop,king,queen,bishop,knight,rook,pawn,pawn,
%%  pawn,pawn,pawn,pawn,pawn,pawn,empty,empty,empty,empty,empty,
%%  empty,empty,empty,empty,empty,empty,empty,empty|...]
%%
%% 4> sc_list:histograph(ChessBoard).
%% [ { bishop, 4  },
%%   { empty,  32 },
%%   { king,   2  },
%%   { knight, 4  },
%%   { pawn,   16 },
%%   { queen,  2  },
%%   { rook,   4  } ]'''
%%
%% @todo add an argument presort to this and other functions to skip the sorting pass

-spec histograph(List::list()) -> weight_list(_T).

histograph([]) ->

    [];





histograph(List)

    when is_list(List) ->

    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).





%% @private

histo_count( [], Current, Count, Work) ->

     lists:reverse( [{Current,Count}] ++ Work);





histo_count( [Current|Tail], Current, Count, Work) ->

    histo_count(Tail, Current, Count+1, Work);





histo_count( [New|Tail], Current, Count, Work) ->

    histo_count(Tail, New, 1, [{Current,Count}] ++ Work).





%% @equiv intersection(List1, List2, unsorted)

intersection(List1, List2) ->

    intersection(List1, List2, unsorted).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Efficiently computes the intersection of two lists.  The third parameter, which is optional and defaults to `unsorted', is either the atom `sorted' or `unsorted'.  If `sorted' is used, the function will sort both inputs before proceeding, as it requires sorted lists; as such, if you already know your lists to be sorted, passing `unsorted' will save some time.  The return list will be reverse sorted. ```1> sc_list:intersection([1,2,3,4,5,2,3,10,15,25,30,40,45,55],[1,3,5,5,5,15,20,30,35,40,50,55]).
%% [55,40,30,15,5,3,1]
%%
%% 2> sc_list:intersection([1],[2]).
%% []''' {@section Thanks} to Ayrnieu for catching a defect in the initial implementation.
%%
%% @since Version 471

-spec intersection(List1::list(), List2::list(), IsSorted::sorted|unsorted) -> list().

intersection(List1, List2, unsorted) ->

    intersection(lists:sort(List1), lists:sort(List2), sorted);





intersection(List1, List2, sorted) ->

    intersect_walk(List1, List2, []).





%% @private

intersect_walk( [], _L2, Work ) ->

    Work;





intersect_walk( _L1, [], Work) ->

    Work;





intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work)

    when L1Head == L2Head ->

    intersect_walk(L1Rem, L2Rem, [L1Head]++Work);





intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work)

    when L1Head < L2Head ->

    intersect_walk(L1Rem, [L2Head|L2Rem], Work);





intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work)

    when L1Head > L2Head ->

    intersect_walk( [L1Head|L1Rem], L2Rem, Work).
