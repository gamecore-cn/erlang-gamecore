%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 14:34
%%%-------------------------------------------------------------------
-module(elist).
-author("xiayiping").
-include("common.hrl").

-export([
    is_keylist/1,
    merge_key_list/3,
    key_find/3,
    key_find/4,
    key_find_m/2,
    key_find_m/3,
    key_search/2,
    key_search/3,
    key_match/3,
    key_list_map/2,
    key_find_last/3,
    key_find_last/4,
    key_find_first/3,
    key_find_first/4,
    keys_delete/3,
    identical/2,
    include/2,
    include_list/2,
    delete_if/2,
    fmax/2,
    index_of/2
]).

%% @doc
%% @end
fmax(_, []) -> error;
fmax(Fun, List) ->
    [First | Rest] = List,
    lists:foldl(fun
                    (Element, Max) ->
                        case Fun(Element) > Fun(Max) of
                            true -> Element;
                            false -> Max
                        end
                end, First, Rest).

%% @doc
%% Return true if the given list is a keylist
%% @end
is_keylist(L) when is_list(L) ->
    lists:all(fun(E) -> is_tuple(E) andalso tuple_size(E) =:= 2 end, L).

%% @doc
%% Merge the two keylists.
%%
%% Example:
%% <pre>
%% Args = [{a, 1}, {b, 2}],
%% Default = [{b, 3}, {c, 4}],
%% elist:merge_key_list(1, Args, Default),
%%   #=> [{c, 4}, {a, 1}, {b, 2}]
%% </pre>
%% @end
merge_key_list(_, [], TupleList2) ->
    TupleList2;
merge_key_list(N, [Tuple | Rest], TupleList2) when
    is_integer(N), is_list(TupleList2), is_tuple(Tuple), is_list(Rest) ->
    Key = element(N, Tuple),
    TupleList3 = case lists:keysearch(Key, N, TupleList2) of
                     {value, _} -> lists:keydelete(Key, N, TupleList2);
                     false -> TupleList2
                 end,
    merge_key_list(N, Rest, TupleList3 ++ [Tuple]);
merge_key_list(N, [Tuple | Rest], TupleList2) when
    is_integer(N), is_list(TupleList2), is_list(Rest) ->
    merge_key_list(N, Rest, TupleList2 ++ [Tuple]).

%% @hidden
key_find(Key, N, List) ->
    key_find(Key, N, List, false).

%% @doc
%% @end
key_find_m(Keys, List) ->
    key_find_m(Keys, List, false).

%% @doc
%% @end
key_find_m(_, [], Default) -> Default;
key_find_m(Keys, [Tuple | Rest], Default) ->
    case lists:all(fun
                       ({Key, N}) ->
                           if
                               size(Tuple) >= N, element(N, Tuple) =:= Key ->
                                   true;
                               true ->
                                   false
                           end
                   end, Keys) of
        true -> Tuple;
        false -> key_find_m(Keys, Rest, Default)
    end.

%% @doc
%% Extended keylistmap for tuple list
%%
%% Example:
%% <pre>
%% Args = [{<<toto>>, world, 2}, {<<titi>>, hello}],
%% Funs = [
%%     {1, erlang:binary_to_list/1},
%%     {3, fun(X) -> X * 2 end}
%%   ],
%% elist:keylistmap(Funs, Args).
%%   # => [{"toto", world, 4}, {"titi", hello}]
%% </pre>
%% @end
key_list_map(Funs, TupleList) when is_list(Funs), is_list(TupleList) ->
    key_list_map(Funs, TupleList, []).

%% @hidden
key_list_map(_, [], Result) -> lists:reverse(Result);
key_list_map(Funs, [Tuple | Rest], Result) ->
    key_list_map(Funs, Rest,
        [list_to_tuple(
            lists:map(fun({N, Data}) ->
                case lists:keyfind(N, 1, Funs) of
                    {N, Fun} -> Fun(Data);
                    false -> Data
                end
                      end,
                [{I, element(I, Tuple)} ||
                    I <- lists:seq(1, tuple_size(Tuple))]))
            | Result]).

%% @doc
%% Save as lists:keyfind/3 but with a default value
%%
%% Example:
%% <pre>
%% Result = elists:keyfind(a, 1, [{b, 2}, {c, 3}], 1).
%%   #=> 1
%% </pre>
%% @end
key_find(Key, N, List, Default) ->
    case lists:keyfind(Key, N, List) of
        {Key, Result} -> Result;
        _ -> Default
    end.

%% @doc
%% Same as elist:key_find_first/4 where Default = ?undefined
%% @end
key_find_first(Keys, N, List) when is_list(Keys) ->
    key_find_first(Keys, N, List, ?undefined).

%% @doc
%% @end
key_find_first([], _, _, Default) ->
    Default;
key_find_first([Key | Keys], N, List, Default) ->
    case lists:keyfind(Key, N, List) of
        {Key, Value} -> {Key, Value};
        _ -> key_find_first(Keys, N, List, Default)
    end.

%% @doc
%% @end
keys_delete(Keys, N, List) ->
    lists:foldl(fun
                    (Key, Acc) ->
                        lists:keydelete(Key, N, Acc)
                end, List, Keys).

%% @doc
%% @end
key_search(Fun, List) ->
    key_search(Fun, List, ?undefined).

%% @doc
%% @end
key_search(_, [], Default) -> Default;
key_search(Fun, [E | List], Default) ->
    case Fun(E) of
        true -> E;
        false -> key_search(Fun, List, Default)
    end.


%% @doc
%% Same as elist:key_find_last/4 where Default = ?undefined
%% @end
key_find_last(Keys, N, List) when is_list(Keys) ->
    key_find_last(Keys, N, List, ?undefined).

%% @doc
%% @end
key_find_last(Keys, N, List, Default) when is_list(Keys) ->
    key_find_first(lists:reverse(Keys), N, List, Default).

%% @doc
%% @end
key_match(Tuple, N, List) when is_tuple(Tuple), is_tuple(N), is_list(List) ->
    key_match(
        fun(E, T) ->
            {_, Result} = lists:foldl(
                fun(I, {I1, Res}) ->
                    Res1 = case tuple_size(E) >= tuple_size(T) of
                               true ->
                                   Res andalso element(I1, T) =:= element(I, E);
                               false -> Res and false
                           end,
                    {I1 + 1, Res1}
                end, {1, true}, eutil:to_list(N)),
            Result
        end, Tuple, List);
key_match(Fun, Tuple, List) when is_tuple(Tuple), is_function(Fun), is_list(List) ->
    lists:foldl(fun(Element, Acc) ->
        case Fun(Element, Tuple) of
            true -> [Element | Acc];
            false -> Acc
        end
                end, [], List).


%% @doc
%% Return true if the two given lists are identical
%%
%% Two lists are considered identical if they have exactly the same
%% number of same elements.
%% @end
identical(List1, List2) when is_list(List1), is_list(List2) ->
    lists:sort(List1) =:= lists:sort(List2).

%% @doc
%% Return true if E is in List
%% @end
include(List, E) when is_list(List) ->
    lists:member(E, List).

%% @doc
%% Return true if all element in IncList are in List
%% @end
include_list(List, IncList) when is_list(List), is_list(IncList) ->
    lists:all(fun
                  (Inc) ->
                      include(List, Inc)
              end, IncList).


%% @doc
%% @end
delete_if(Fun, List) ->
    lists:reverse(lists:foldl(fun
                                  (E, Acc) ->
                                      case Fun(E) of
                                          true -> Acc;
                                          false -> [E | Acc]
                                      end
                              end, [], List)).

%% @doc
%% Return the index of the item in list, or not_found
%% @end
index_of(List, Item) when is_list(List) ->
    index_of(Item, List, 1).


index_of(_, [], _) -> ?not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).
