%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 18. 11月 2021 20:22
%%%-------------------------------------------------------------------
-module(iproxy).
-author("xiayiping").
-include("common.hrl").

%% API
-export([]).

%% @doc Get the proxy name
-callback name(Request :: term(), State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}.

%% @doc 收到其它任何消息时调用
-callback do_info(Info :: term(), State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}.

%% @doc 循环定时调用(心跳等)
-callback do_loop(State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}.