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
%% @doc 进程之间调用
-define(GAME_SERVER_TIMEOUT, 5000).   % gen_server默认timeout 默认5秒

%% @doc 进程之间函数调用
-define(func, '$f').                     % func
-define(func_state, '$fs').              % func_state
-define(no_block_call_return, '$ncr').   % no_block_call_return
-define(no_block_call, '$nc').           % no_block_call

%% @doc 错误码  这个需要在 pp_xxx 做对应抛出的捕获
-define(C2S_ERROR(Error), case Error of {?error, _} -> throw(Error); _ -> throw({?error, Error}) end).

%% @doc Fun
-define(FUN_INFO(Pid, Fun, Args), erlang:send(Pid, {?func, Fun, Args})).                           % Args     数据，请尽量的小    忽律函数Fun的返回  参数开头不加State
-define(FUN_CAST(Pid, Fun, Args), gen_server:cast(Pid, {?func, Fun, Args})).                       % Args     数据，请尽量的小    忽律函数Fun的返回  参数开头不加State
-define(FUN_CALL(Pid, Fun, Args), gen_server:call(Pid, {?func, Fun, Args}, ?GAME_SERVER_TIMEOUT)). % Args 返回 数据，请尽量的小    忽律函数Fun的返回  参数开头不加State

-define(FUN_INFO_STATE(Pid, Fun, Args), erlang:send(Pid, {?func_state, Fun, Args})).                           % Args     数据，请尽量的小   函数Fun返回：?ok | {?ok, NewState}
-define(FUN_CAST_STATE(Pid, Fun, Args), gen_server:cast(Pid, {?func_state, Fun, Args})).                       % Args     数据，请尽量的小   函数Fun返回：?ok | {?ok, NewState}
-define(FUN_CALL_STATE(Pid, Fun, Args), gen_server:call(Pid, {?func_state, Fun, Args}, ?GAME_SERVER_TIMEOUT)). % Args 返回 数据，请尽量的小   函数Fun返回：?ok | {?ok, Reply, NewState}

%% 封装处理try ... catch
-define(TRY_SRV_DO(Fun, FunArgs, Mod, State, FuncState),
    try
        FunArgs2 =
            case FuncState of
                ?func_state -> FunArgs ++ [State];
                _ -> FunArgs
            end,
        case erlang:apply(Fun, FunArgs2) of
            ?ok -> {?noreply, {Mod, State}};
            {?ok, NewState} -> {?noreply, {Mod, NewState}};
            {?noreply, NewState} -> {?noreply, {Mod, NewState}};
            {?ok, Reply, NewState} -> {?reply, Reply, {Mod, NewState}};
            {?reply, Reply, NewState} -> {?reply, Reply, {Mod, NewState}};
            {?stop, Reason, NewState} -> {?stop, Reason, {Mod, NewState}};
            {?stop, Reason, Reply, NewState} -> {?stop, Reason, Reply, {Mod, NewState}};
            ApplyError ->
                ?ERROR("---------------------------------
Mod  :~w
Args :~w
Error:~w
FuncState:~w
---------------------------------", [Mod, FunArgs, ApplyError, FuncState]),
                {?noreply, {Mod, State}}
        end
    catch
        catch _SrvTryClass:_SrvTryError:_SrvTryStacktrace ->
            ?ERROR("---------------------------------
Fun  :~w
Args :~w

Class:~w
Error:~w
Trace:~p

Mod  :~w
State:~w
FuncState:~w
---------------------------------", [Fun, FunArgs, _SrvTryClass, _SrvTryError, _SrvTryStacktrace, Mod, State, FuncState]),
            {?noreply, {Mod, State}}
    end).

%%% record
%%% -------------------------------------------------------------------
%% @doc 通知 notification
-record(n, {
    name :: atom()      % 名称
    , type :: integer() % 类型
    , body :: term()    % 通知内容
}).

%% @doc 事件监听的信号 signal
-record(s, {
    id :: atom()              % 信号id,同协议号的区间并要求在对应模块*.hrl里用常量定好  类型:unit32()
    , target_id :: integer()  % 目标模块id，同模块协议区间 在common.hrl常量定义好  类型:unit32()
    , data :: term()          % 数据，请尽量的小，不要转#role{}等大块数。大的数据可以通过读ets来实现
}).

-endif.