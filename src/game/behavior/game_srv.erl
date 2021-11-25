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
-include("common.hrl").
-include("srv.hrl").


-behaviour(gen_server).

-export([
    %% API
    start_link/1, name/0, stop_link/2, call_no_block/4
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
-callback do_loop(Args :: term(), State :: term()) ->
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
    case ?TRY(Mod:do_init(Args), ?normal) of
        {?ok, State} ->
            ?ECHO_MODULE_START(Mod),
            {?ok, {Mod, State}};
        {?stop, Reason} ->
            ?ERROR("Mod:~w Args:~w Reason:~w", [Mod, Args, Reason]),
            {?stop, Reason};
        Error ->
            ?ERROR("Mod:~w Args:~w  Error:~w", [Mod, Args, Error]),
            {?stop, Error}
    end.
handle_call({?func, F, A}, _From, {Mod, State}) ->
    ?TRY_SRV_DO(F, A, Mod, State, ?func);
handle_call({?func_state, F, A}, _From, {Mod, State}) ->
    ?TRY_SRV_DO(F, A, Mod, State, ?func_state);
handle_call(Request, From, {Mod, State}) ->
    ?TRY_SRV_DO(fun Mod:do_call/3, [Request, From], Mod, State, ?func_state).

handle_cast({?func, F, A}, {Mod, State}) ->
    ?TRY_SRV_DO(F, A, Mod, State, ?func);
handle_cast({?func_state, F, A}, {Mod, State}) ->
    ?TRY_SRV_DO(F, A, Mod, State, ?func_state);
handle_cast({?stop, Reason}, {Mod, State}) ->
    NewState = stop(Mod, State, Reason),
    {?stop, Reason, {Mod, NewState}};
handle_cast(Request, {Mod, State}) ->
    ?TRY_SRV_DO(fun Mod:do_cast/2, [Request], Mod, State, ?func_state).

handle_info(?loop, {Mod, State}) ->
    ?TRY_SRV_DO(fun Mod:do_loop/2, [?loop], Mod, State, ?func_state);
handle_info({?loop, Args}, {Mod, State}) ->
    ?TRY_SRV_DO(fun Mod:do_loop/2, [Args], Mod, State, ?func_state);
handle_info(#s{} = Signal, {Mod, State}) ->
    ?TRY_SRV_DO(fun Mod:do_event/2, [Signal], Mod, State, ?func_state);
handle_info({?func, F, A}, {Mod, State}) ->
    ?TRY_SRV_DO(F, A, Mod, State, ?func);
handle_info({?func_state, F, A}, {Mod, State}) ->
    ?TRY_SRV_DO(F, A, Mod, State, ?func_state);
handle_info({?no_block_call, {From, FromRef, Fun, Args}, Timeout}, State) ->
    case etimer:now_millisec() >= Timeout of
        ?true ->
            erlang:send(From, {?no_block_call_return, FromRef, ?timeout});
        ?false ->
            call_no_block_return(From, FromRef, Fun, Args)
    end,
    {?noreply, State};
handle_info(Info, {Mod, State}) ->
    ?TRY_SRV_DO(fun Mod:do_info/2, [Info], Mod, State, ?func_state).

code_change(OldVsn, {Mod, State}, Extra) ->
    case erlang:function_exported(Mod, code_change, 3) of
        ?true ->
            case ?TRY(Mod:code_change(OldVsn, State, Extra), ?ok) of
                {?ok, NewState} ->
                    {?ok, {Mod, NewState}};
                ?ok ->
                    {?ok, {Mod, State}};
                Error2 ->
                    ?ERROR("Mod:~w  Args:~w  Error:~w", [Mod, {OldVsn, Extra}, Error2]),
                    Error2
            end;
        _ ->
            {?ok, {Mod, State}}
    end.

terminate(Reason, {Mod, State}) ->
    NewState = stop(Mod, State, Reason),
    case erlang:function_exported(Mod, terminate, 2) of
        ?true ->
            ?TRY(Mod:terminate(Reason, NewState), ?ok);
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
                    ?TRY(Mod:do_stop(Reason, State), State);
                _ ->
                    State
            end
    end.

%% @doc 同步非阻塞调用，避免死锁
%% 需注意，该逻辑破坏了操作的原子性，应当保证向别人请求的call和收到别人的call不处理同一份数，否则会出现数据错误
call_no_block(Pid, Fun, Args, Timeout) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        ?true ->
            FromPid = self(),
            FromRef = erlang:make_ref(),
            NowMillisecond = etimer:now_millisec(),
            Timeout2 = NowMillisecond + erlang:ceil(Timeout),
            erlang:send(Pid, {?no_block_call, {FromPid, FromRef, Fun, Args}, Timeout2}),
            case call_no_block_receive(FromRef, Timeout2) of
                ?timeout -> ?timeout;
                Ret -> Ret
            end;
        ?false ->
            {?error, {Pid, process_not_alive}}
    end.

%% @private
call_no_block_receive(Ref, Timeout) ->
    NowMillisecond = etimer:now_millisec(),
    case NowMillisecond >= Timeout of
        ?true -> ?timeout;
        ?false ->
            Diff = Timeout - NowMillisecond,
            receive
                {?no_block_call_return, Ref, RetValue} ->
                    RetValue;
                {?no_block_call, {From, FromRef, Fun, Args}, NowMillisecond2} ->
                    case NowMillisecond >= NowMillisecond2 of
                        ?true ->
                            erlang:send(From, {?no_block_call_return, FromRef, ?timeout});
                        ?false ->
                            call_no_block_return(From, FromRef, Fun, Args),
                            call_no_block_receive(Ref, Timeout)
                    end
            after Diff ->
                ?timeout
            end
    end.

%% @private
call_no_block_return(From, FromRef, Fun, Args) ->
    RetValue = ?TRY(erlang:apply(Fun, Args), ?ok),
    erlang:send(From, {?no_block_call_return, FromRef, RetValue}),
    ?ok.

