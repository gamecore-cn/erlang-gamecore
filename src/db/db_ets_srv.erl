%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 19. 11月 2021 18:29
%%%-------------------------------------------------------------------
-module(db_ets_srv).
-author("xiayiping").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(db_ets_srv_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #db_ets_srv_state{}} | {ok, State :: #db_ets_srv_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #db_ets_srv_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #db_ets_srv_state{}) ->
    {reply, Reply :: term(), NewState :: #db_ets_srv_state{}} |
    {reply, Reply :: term(), NewState :: #db_ets_srv_state{}, timeout() | hibernate} |
    {noreply, NewState :: #db_ets_srv_state{}} |
    {noreply, NewState :: #db_ets_srv_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #db_ets_srv_state{}} |
    {stop, Reason :: term(), NewState :: #db_ets_srv_state{}}).
handle_call(_Request, _From, State = #db_ets_srv_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #db_ets_srv_state{}) ->
    {noreply, NewState :: #db_ets_srv_state{}} |
    {noreply, NewState :: #db_ets_srv_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #db_ets_srv_state{}}).
handle_cast(_Request, State = #db_ets_srv_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #db_ets_srv_state{}) ->
    {noreply, NewState :: #db_ets_srv_state{}} |
    {noreply, NewState :: #db_ets_srv_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #db_ets_srv_state{}}).
handle_info(_Info, State = #db_ets_srv_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #db_ets_srv_state{}) -> term()).
terminate(_Reason, _State = #db_ets_srv_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #db_ets_srv_state{},
    Extra :: term()) ->
    {ok, NewState :: #db_ets_srv_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #db_ets_srv_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
