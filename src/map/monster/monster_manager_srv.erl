%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2021 15:33
%%%-------------------------------------------------------------------
-module(monster_manager_srv).
-author("xiayiping").
-include("common.hrl").
-include("srv.hrl").

-behaviour(game_srv).

%% API
-export([start_link/1, do_init/1, do_call/3, do_cast/2, do_info/2, do_loop/2, do_event/2, do_stop/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 关联开始gen_server并注册服务器(唯一)名称
start_link(Args) ->
    game_srv:start_link([?MODULE | Args]).

%% @doc 进程初始化
-spec(do_init(Args :: term()) ->
    {?ok, State :: term()} |
    {?stop, Reason :: term()}).
do_init(_Args) ->
    {?ok, #state{}}.

%% @doc GenServer Call 消息时调用(少用)
-spec(do_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) ->
    {?reply, Reply :: term(), NewState :: term()} |
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}).
do_call(_Request, _From, State) ->
    {?reply, ?ok, State}.

%% @doc GenServer Cast 消息时调用
-spec(do_cast(Request :: term(), State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}).
do_cast(_Request, State) ->
    {?noreply, State}.

%% @doc 收到其它任何消息时调用
-spec(do_info(Info :: term(), State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}).
do_info(_Info, State) ->
    {?noreply, State}.

%% @doc 循环定时调用(心跳等)
-spec(do_loop(Args :: term(), State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}).
do_loop(_Args, State) ->
    {?noreply, State}.

%% @doc 收到事件消息时调用
-spec(do_event(Signal :: #s{}, State :: term()) ->
    {?noreply, NewState :: term()} |
    {?stop, Reason :: term(), NewState :: term()}).
do_event(_Signal, State) ->
    {?noreply, State}.

%% @doc 停止或退出时调用，在terminate之前调用用
-spec(do_stop(State :: term(), Reason :: term()) -> NewState :: term()).
do_stop(State, _Reason) ->
    State.

%% @private
%% @doc 函数在gen_server将要调用时被调用终止。
%%      它应该与Module:init/1相反，并执行任何操作必要的清理。当它返回时，gen_server终止与原因。返回值被忽略。
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ?ok.

%% @private
%% @doc 热更新代码时 更新State状态
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
    {?ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {?ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
