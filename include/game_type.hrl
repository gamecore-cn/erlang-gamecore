%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 16:16
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__game_type__hrl).
-define(__game_type__hrl, true).

%%% define
%%% -------------------------------------------------------------------


%%% type
%%% -------------------------------------------------------------------
%% 奖励类型
-type reward_type() :: {recruit, ItemId :: integer(), ItemNum :: integer(), Expend :: integer(), Star :: integer()}
| {_, ItemId :: integer(), ItemNum :: integer()}
| {ItemId :: integer(), {_, ItemNum :: integer()}}
| {ItemId :: integer(), ItemNum :: integer()}.



-endif.