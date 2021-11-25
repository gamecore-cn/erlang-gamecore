%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 24. 11月 2021 10:27
%%%-------------------------------------------------------------------
-module(gateway_acceptor_sup).
-author("xiayiping").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gateway_acceptor_sup_state, {}).

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
    {ok, State :: #gateway_acceptor_sup_state{}} | {ok, State :: #gateway_acceptor_sup_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #gateway_acceptor_sup_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #gateway_acceptor_sup_state{}) ->
    {reply, Reply :: term(), NewState :: #gateway_acceptor_sup_state{}} |
    {reply, Reply :: term(), NewState :: #gateway_acceptor_sup_state{}, timeout() | hibernate} |
    {noreply, NewState :: #gateway_acceptor_sup_state{}} |
    {noreply, NewState :: #gateway_acceptor_sup_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #gateway_acceptor_sup_state{}} |
    {stop, Reason :: term(), NewState :: #gateway_acceptor_sup_state{}}).
handle_call(_Request, _From, State = #gateway_acceptor_sup_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #gateway_acceptor_sup_state{}) ->
    {noreply, NewState :: #gateway_acceptor_sup_state{}} |
    {noreply, NewState :: #gateway_acceptor_sup_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gateway_acceptor_sup_state{}}).
handle_cast(_Request, State = #gateway_acceptor_sup_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gateway_acceptor_sup_state{}) ->
    {noreply, NewState :: #gateway_acceptor_sup_state{}} |
    {noreply, NewState :: #gateway_acceptor_sup_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gateway_acceptor_sup_state{}}).
handle_info(_Info, State = #gateway_acceptor_sup_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #gateway_acceptor_sup_state{}) -> term()).
terminate(_Reason, _State = #gateway_acceptor_sup_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gateway_acceptor_sup_state{},
    Extra :: term()) ->
    {ok, NewState :: #gateway_acceptor_sup_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gateway_acceptor_sup_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
