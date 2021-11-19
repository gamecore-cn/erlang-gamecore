%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 19. 11月 2021 14:15
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__srv__hrl).
-define(__srv__hrl, true).
-include("common.hrl").

%%% define
%%% -------------------------------------------------------------------
%% @doc 错误码  这个需要在 pp_xxx 做对应抛出的捕获
-define(C2S_ERROR(Error), case Error of {?error, _} -> throw(Error); _ -> throw({?error, Error}) end).

%% @doc 进程之间调用
-define(GAME_SERVER_TIMEOUT, 5000).   % gen_server默认timeout 默认5秒

%% @doc 进程之间函数调用
-define(FUN, f).                     % exec_cast / exec_call / exec_info

-define(FUN_INFO(Pid, Fun, Args), erlang:send(Pid, {?FUN, Fun, Args})).                           % Args 数据，请尽量的小       函数Fun返回：?ok | {?ok, NewState}
-define(FUN_CAST(Pid, Fun, Args), gen_server:cast(Pid, {?FUN, Fun, Args})).                       % Args 数据，请尽量的小       函数Fun返回：?ok | {?ok, NewState}
-define(FUN_CALL(Pid, Fun, Args), gen_server:call(Pid, {?FUN, Fun, Args}, ?GAME_SERVER_TIMEOUT)). % Args 返回 数据，请尽量的小   函数Fun返回：?ok | {?ok, Reply, NewState}

-define(FUN_INFO_NORETURN(Pid, Fun, Args), erlang:send(Pid, {?FUN, Fun, {?noreturn, Args}})).                           % Args 数据，请尽量的小       忽律函数Fun的返回，参数开头不加State
-define(FUN_CAST_NORETURN(Pid, Fun, Args), gen_server:cast(Pid, {?FUN, Fun, {?noreturn, Args}})).                       % Args 数据，请尽量的小       忽律函数Fun的返回，参数开头不加State
-define(FUN_CALL_NORETURN(Pid, Fun, Args), gen_server:call(Pid, {?FUN, Fun, {?noreturn, Args}}, ?GAME_SERVER_TIMEOUT)). % Args 返回 数据，请尽量的小   忽律函数Fun的返回，参数开头不加State



-endif.