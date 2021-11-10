%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 16:49
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__log__hrl).
-define(__log__hrl, true).

-define(NOTICE(Format), ?NOTICE(Format, [])).
-define(DEBUG(Format), ?DEBUG(Format, [])).
-define(INFO(Format), ?INFO(Format, [])).
-define(WARNING(Format), ?WARNING(Format, [])).
-define(ERROR(Format), ?ERROR(Format, [])).

-ifdef(LAGER_MM_LOG).
    -define(NOTICE(Format, Args), lager_log:notice_msg(?MODULE, ?LINE, Format, Args)).
    -define(DEBUG(Format, Args), lager_log:debug_msg(?MODULE, ?LINE, Format, Args)).
    -define(INFO(Format, Args), lager_log:info_msg(?MODULE, ?LINE, Format, Args)).
    -define(WARNING(Format, Args), lager_log:warning_msg(?MODULE, ?LINE, Format, Args)).
    -define(ERROR(Format, Args), lager_log:error_msg(?MODULE, ?LINE, Format, Args)).
-else.
    -define(NOTICE(Format, Args), io:format("[~p]~p:~p " ++ Format ++ "~n", [debug, ?MODULE, ?LINE | Args])).
    -define(DEBUG(Format, Args), io:format("[~p]~p:~p " ++ Format ++ "~n", [debug, ?MODULE, ?LINE | Args])).
    -define(INFO(Format, Args), io:format("[~p]~p:~p " ++ Format ++ "~n", [info, ?MODULE, ?LINE | Args])).
    -define(WARNING(Format, Args), io:format("[~p]~p:~p " ++ Format ++ "~n", [warning, ?MODULE, ?LINE | Args])).
    -define(ERROR(Format, Args), io:format("[~p]~p:~p " ++ Format ++ "~n", [error, ?MODULE, ?LINE | Args])).
-endif.

-endif.