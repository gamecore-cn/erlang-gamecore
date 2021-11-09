%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 12:32
%%%-------------------------------------------------------------------
-module(icontroller).
-author("xiayiping").
-include("common.hrl").
-include("mvc.hrl").

%% callback
%%------------------------------------------------------------------
%% @doc
-callback register(NotificationName :: atom(),  CommandModule::atom()) ->
    ?ok.

%% @doc
-callback execute(Notification :: #i_notification{}) ->
    ?ok.

%% @doc 删除已注册的`命令ICommand`到`通知Notification`映射。
-callback remove(NotificationName :: atom()) ->
    ?ok.

%% @doc 检查`命令ICommand`是否已注册`通知Notification`
-callback has(NotificationName :: atom()) ->
    ?ok.

