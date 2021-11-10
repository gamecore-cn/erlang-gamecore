%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 16:56
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__common__hrl).
-define(__common__hrl, true).
-include("log.hrl").
-include("game_errorcode.hrl").

%%% define 常用常量
%%% -------------------------------------------------------------------
%% 时间相关
-define(SECOND_WEEK, 604800).% 7 * 86400).		% 每周有多少秒
-define(SECOND_DAY, 86400).                     % 每天有多少秒
-define(SECOND_HOUR, 3600).                     % 每小时有多少秒
-define(SECOND_MINUTES, 60).                    % 每分钟有多少秒

-define(SECOND_WEEK(N), trunc(?SECOND_WEEK * (N))).
-define(SECOND_DAY(N), trunc(?SECOND_DAY * (N))).
-define(SECOND_HOUR(N), trunc(?SECOND_HOUR * (N))).
-define(SECOND_MINUTES(N), trunc(?SECOND_MINUTES * (N))).

-define(SECOND_WEEK_MS, 604800000).
-define(SECOND_DAY_MS, 86400000).
-define(SECOND_HOUR_MS, 3600000).
-define(SECOND_MINUTES_MS, 60000).
-define(SECOND_MS, 1000).

-define(SECOND_WEEK_MS(N), trunc(?SECOND_WEEK_MS * (N))).
-define(SECOND_DAY_MS(N), trunc(?SECOND_DAY_MS * (N))).
-define(SECOND_HOUR_MS(N), trunc(?SECOND_HOUR_MS * (N))).
-define(SECOND_MINUTES_MS(N), trunc(?SECOND_MINUTES_MS * (N))).
-define(SECOND_MS(N), trunc(?SECOND_MS * (N))).

-define(SECOND_YEAR_THREE, 31536000).           % 按照365天计算的三年秒数

-define(TIME_TYPE_WEEK, 4).   % 周
-define(TIME_TYPE_DAY, 3).    % 天
-define(TIME_TYPE_HOUR, 2).   % 小时
-define(TIME_TYPE_MINU, 1).   % 分
-define(TIME_TYPE_SECS, 0).   % 秒

-define(MS_TO_SEC(N), (N div 1000)).

% 常用常量
-define(DATA_VALUE, 1).     % 值
-define(DATA_PERCENT, 2).   % 百分比

-define(UINT_32_MAX, 16#FFFFFFFF). % 无符号int32最大值
-define(INT_32_MAX, 16#7FFFFFFF).  % 有符号int32最大值

-define(TRUE_INT, 1).            % 1表示true
-define(FALSE_INT, 0).           % 0表示false

%% 常用atom定义
-define(TRUE, true).             % true
-define(FALSE, false).           % false
-define(true, true).             % true
-define(false, false).           % false
-define(value, value).           % value
-define(ok, ok).                 % ok
-define(error, error).           % error
-define(fail, fail).             % fail
-define(start, start).           % start
-define(stop, stop).             % stop
-define(reply, reply).           % reply
-define(loop, loop).             % loop
-define(noreply, noreply).       % noreply
-define(ignore, ignore).         % ignore
-define(skip, skip).             % skip
-define(undefined, undefined).   % undefined
-define(none, none).             % none
-define(NONE, none).             % none
-define(normal, normal).         % normal
-define(exit, exit).             % exit
-define(trap_exit, trap_exit).   % trap_exit
-define(inet_reply, inet_reply). % inet_reply
-define(func, func).             % func
-define(add, add).               % add
-define(del, del).               % del
-define(update, update).         % update
-define(not_found, not_found).   % not_found
-define(no_data, no_data).       % no_data
-define(noreturn, noreturn).     % noreturn


%% 进程之间调用
-define(GAME_SERVER_TIMEOUT, 5000).   % gen_server默认timeout 默认5秒

-define(FUNC, f).              % exec_cast / exec_call / exec_info

-define(FUNC_INFO(Pid, Fun, Args), erlang:send(Pid, {?FUNC, Fun, Args})).                           % Args 数据，请尽量的小       函数Fun返回：?ok | {?ok, NewState}
-define(FUNC_CAST(Pid, Fun, Args), gen_server:cast(Pid, {?FUNC, Fun, Args})).                       % Args 数据，请尽量的小       函数Fun返回：?ok | {?ok, NewState}
-define(FUNC_CALL(Pid, Fun, Args), gen_server:call(Pid, {?FUNC, Fun, Args}, ?GAME_SERVER_TIMEOUT)). % Args 返回 数据，请尽量的小   函数Fun返回：?ok | {?ok, Reply, NewState}
-define(FUNC_INFO_NORETURN(Pid, Fun, Args), erlang:send(Pid, {?FUNC, Fun, {?noreturn, Args}})).                           % Args 数据，请尽量的小       忽律函数Fun的返回，参数开头不加State
-define(FUNC_CAST_NORETURN(Pid, Fun, Args), gen_server:cast(Pid, {?FUNC, Fun, {?noreturn, Args}})).                       % Args 数据，请尽量的小       忽律函数Fun的返回，参数开头不加State
-define(FUNC_CALL_NORETURN(Pid, Fun, Args), gen_server:call(Pid, {?FUNC, Fun, {?noreturn, Args}}, ?GAME_SERVER_TIMEOUT)). % Args 返回 数据，请尽量的小   忽律函数Fun的返回，参数开头不加State
-define(FUNC_DO(State, Fun, Args),
    case Args of
        {?noreturn, Args2} ->
            erlang:apply(Fun, Args2),
            {?noreply, State};
        _ ->
            case erlang:apply(Fun, [State | Args]) of
                ?ok -> {?noreply, State};
                {?ok, NewState} -> {?noreply, NewState};
                {?noreply, NewState} -> {?noreply, NewState};
                {?ok, Reply, NewState} -> {?reply, Reply, NewState};
                {?reply, Reply, NewState} -> {?reply, Reply, NewState};
                {?stop, Reason, NewState} -> {?stop, Reason, NewState};
                {?stop, Reason, Reply, NewState} -> {?stop, Reason, Reply, NewState};
                TryError ->
                    ?ERROR(" FUNC_ERROR
Funs :~w
Args :~w
Error:~p
State:~w", [Fun, Args, TryError, State]),
                    {?noreply, State}
            end
    end).

%% 错误码  这个需要在 pp_xxx 做对应抛出的捕获
-define(C2S_ERROR(Error), case Error of {?error, _} -> throw(Error); _ -> throw({?error, Error}) end).

%% 异常
-define(TRY(CODE),
    try
        CODE
    catch _CodeTryClass:_CodeTryError:_CodeTryStacktrace ->
        ?ERROR("
Class:~w
Error:~w
Stack:~p", [_CodeTryClass, _CodeTryError, _CodeTryStacktrace]),
        ?ok
    end).
-define(TRY(Fun, DefaultReturn), ?TRY(Fun, [], DefaultReturn)).
-define(TRY(Fun, Args, DefaultReturn),
    try
        erlang:apply(Fun, Args)
    catch _FunTryClass:_FunTryError:_FunTryStacktrace ->
        ?ERROR("
Class:~w
Error:~w
Stack:~p", [_FunTryClass, _FunTryError, _FunTryStacktrace]),
        DefaultReturn
    end).
-define(TRY_GATEWAY(Fun, Args, ErrorFun, ErrorFunArgs),
    try
        erlang:apply(Fun, Args)
    catch _PpTryClass:_PpTryError:_PpTryStacktrace ->
        _PpTryErrorCode =
            case _PpTryError of
                {?error, _PpTryErrorCode00} -> _PpTryErrorCode00;
                _PpTryErrorCode00 when is_integer(_PpTryErrorCode00) -> _PpTryErrorCode00;
                _PpTryError00 ->
                    ?ERROR("
Class:~w
Error:~w
Stack:~p", [_PpTryClass, _PpTryError00, _PpTryStacktrace]),
                    ?ERR_SYSTEM
            end,
        erlang:apply(ErrorFun, [_PpTryErrorCode | ErrorFunArgs])
    end).

%% IF
-define(IF(Expr, True, False), case Expr of true -> True; false -> False end).
-define(IF_NOT_EMPTY(Expr, True, False), case Expr of [_ | _] -> True; _ -> False end).
-define(IF_CATCH(Expr, True, False), case Expr of true -> True; false -> erlang:throw(False) end).

-define(IF_REAL, fun echeck:real/2).                      % true or false，true返回ok
-define(IF_ALL_EQUAL, fun echeck:all_equal/3).            % =:=
-define(IF_NEGATIVE, fun echeck:negative/3).              % =/=
-define(IF_EQUAL, fun echeck:equal/3).                    % ==
-define(IF_BIGGER_EQUAL, fun echeck:bigger_equal/3).      % >=
-define(IF_LESS_EQUAL, fun echeck:less_equal/3).          % =<
-define(IF_BIGGER, fun echeck:bigger/3).                  % >
-define(IF_LESS, fun echeck:less/3).                      % <

%%% record 记录
%%% -------------------------------------------------------------------
%% 事件监听的信号
-record(signal, {
    id           % 信号id,同协议号的区间并要求在对应模块*.hrl里用常量定好  类型:unit32()
    , target_id  % 目标模块id，同模块协议区间 在common.hrl常量定义好  类型:unit32()
    , data       % 数据，请尽量的小，不要转#role{}等大块数。大的数据可以通过读ets来实现
}).


-endif.