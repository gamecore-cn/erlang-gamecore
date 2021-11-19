%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 16:11
%%%-------------------------------------------------------------------
-module(game_srv).
-author("xiayiping").

-behaviour(gen_server).

-include("common.hrl").
-include("mvc.hrl").


-export([
    %% API
    start_link/1, name/0, stop_link/2
    %% gen_server callbacks
    , init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).


%%%=========================================================================
%%%  callback
%%%=========================================================================
-callback start(Args :: term()) ->
    ?ok | {?error, Error :: term()}.
-callback stop(Args :: term()) ->
    ?ok | {?error, Error :: term()}.

%% @doc 进程初始化
-callback do_init(Args :: term()) ->
    {?ok, State :: term()} |
    {?stop, Reason :: term()}.

%% @doc GenServer Call 消息时调用(少用)
-callback do_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) ->
    {?reply, Reply :: term(), NewState :: term()} |
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}.

%% @doc GenServer Cast 消息时调用
-callback do_cast(Request :: term(), State :: term()) ->
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

%% @doc 收到事件消息时调用
-callback do_event(Signal :: #s{}, State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}.

%% @doc 停止或退出时调用，在terminate之前调用用
-callback do_stop(State :: term(), Reason :: term()) ->
    NewState :: term().

%% @doc 代码热更新时调用
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(), Extra :: term()) ->
    {?ok, NewState :: term()} |
    {?error, Reason :: term()}.

%% @doc 进程退出时调用
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: term()) ->
    term().

-optional_callbacks([do_stop/2, code_change/3, terminate/2, start/1, stop/1]).


%%%===================================================================
%%% API
%%%===================================================================
%% @doc 通用服务启动入口
-spec(start_link(Args :: list()) ->
    {?ok, Pid :: pid()} | ?ignore | {?error, Reason :: term()}).
start_link([Mod | _] = Args) when is_atom(Mod) ->
    gen_server:start_link({?local, Mod}, ?MODULE, Args, []);
start_link(Args) ->
    ?ERROR("Args Error:~p", [Args]),
    {?error, Args}.

%% @doc 停止服务
-spec(stop_link(Mod :: atom(), Reason :: term()) ->
    {?ok, Pid :: pid()} | ?ignore | {?error, Reason :: term()}).
stop_link(Mod, Reason) when is_atom(Mod) ->
    gen_server:cast(Mod, {?stop, Reason}).

%% @doc 返回当前进程的模块名称。
-spec(name() -> atom()).
name() ->
    get('$name').

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([Mod | Args]) ->
    erlang:put('$name', Mod),
    case ?TRY_CATCH(Mod:do_init(Args)) of
        {?ok, State} ->
            ?ECHO_MODULE_START(Mod),
            {?ok, {Mod, State}};
        {?stop, Reason} ->
            ?ERROR2("Mod:~w Args:~w Reason:~w", [Mod, Args, Reason]),
            {?stop, Reason};
        Error2 ->
            ?ERROR2("Mod:~w Args:~w Error:~w", [Mod, Args, Error2]),
            {?stop, Error2}
    end.
handle_call({?EXEC_CALL, M, F, A} = Request, From, {Mod, State}) ->
    ?TRY_SRV(M:F(State, A), Mod, {Request, From}, State);
handle_call(Request, From, {Mod, State}) ->
    ?TRY_SRV(Mod:do_call(Request, From, State), Mod, {do_call, Request, From}, State).

handle_cast({?EXEC_CAST, M, F, A} = Request, {Mod, State}) ->
    ?TRY_SRV(M:F(State, A), Mod, Request, State);
handle_cast({?stop, Reason}, {Mod, State}) ->
    NewState = stop(Mod, State, Reason),
    {?stop, Reason, {Mod, NewState}};
handle_cast(Request, {Mod, State}) ->
    ?TRY_SRV(Mod:do_cast(Request, State), Mod, {do_cast, Request}, State).

handle_info(?loop, {Mod, State}) ->
    ?TRY_SRV(Mod:do_loop(State), Mod, do_loop, State);
handle_info(#signal{} = Signal, {Mod, State}) ->
    ?TRY_SRV(Mod:do_event(State), Mod, {do_event, Signal}, State);
handle_info(Info, {Mod, State}) ->
    ?TRY_SRV(Mod:do_info(Info, State), Mod, {do_info, Info}, State).

code_change(OldVsn, {Mod, State}, Extra) ->
    case erlang:function_exported(Mod, code_change, 3) of
        ?true ->
            case ?TRY_CATCH(Mod:code_change(OldVsn, State, Extra)) of
                {?ok, NewState} ->
                    {?ok, {Mod, NewState}};
                Error2 ->
                    ?ERROR2("Mod:~w  Args:~w  Error:~w", [Mod, {OldVsn, Extra}, Error2]),
                    Error2
            end;
        _ ->
            {?ok, {Mod, State}}
    end.

terminate(Reason, {Mod, State}) ->
    NewState = stop(Mod, State, Reason),
    case erlang:function_exported(Mod, terminate, 2) of
        ?true ->
            ?TRY_CATCH(Mod:terminate(Reason, NewState));
        _ ->
            ?ok
    end.

stop(Mod, State, Reason) ->
    case get('$already_stop') of
        ?true -> State;
        _ ->
            put('$already_stop', ?true),
            case erlang:function_exported(Mod, do_stop, 2) of
                ?true ->
                    ?TRY_CATCH(Mod:do_stop(Reason, State));
                _ ->
                    State
            end
    end.

