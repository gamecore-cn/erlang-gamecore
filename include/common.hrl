-ifndef(INC_COMMON_HRL).
-define(INC_COMMON_HRL, true).

%% gen_server默认timeout
-define(GEN_SERVER_TIMEOUT, 5000).


%% 进程之间调用
-define(EXEC_CAST, exec_cast).  % exec_cast
-define(EXEC_CALL, exec_call).  % exec_call
-define(EXEC_CAST(Pid, Fun, Arg), gen_server:cast(Pid, {?EXEC_CAST, ?MODULE, Fun, Arg})).                      % Arg 数据，请尽量的小       Fun返回：?ok | {?ok, NewState}
-define(EXEC_CALL(Pid, Fun, Arg), gen_server:call(Pid, {?EXEC_CALL, ?MODULE, Fun, Arg}, ?GEN_SERVER_TIMEOUT)). % Arg、返回 数据，请尽量的小  Fun返回：?ok | {?ok, Reply, NewState}
-define(EXEC_DO(State, M, F, A),
    case M:F(State, A) of
        ?ok -> {?noreply, State};
        {?ok, NewState} -> {?noreply, NewState};
        {?noreply, NewState} -> {?noreply, NewState};
        {?ok, Reply, NewState} -> {?reply, Reply, NewState};
        {?reply, Reply, NewState} -> {?reply, Reply, NewState};
        {?stop, Reason, NewState} -> {?stop, Reason, NewState};
        {?stop, Reason, Reply, NewState} -> {?stop, Reason, Reply, NewState};
        TryError -> ?ERROR("EXEC_DO MFA:~w TryError:~p State:~w", [{M, F, A},TryError,State]), {?noreply, State}
    end).

%% 事件监听的信号
-record(signal, {
    id           % 信号id,同协议号的区间并要求在对应模块*.hrl里用常量定好  类型:unit32()
    , target_id  % 目标模块id，同模块协议区间 在common.hrl常量定义好  类型:unit32()
    , data       % 数据，请尽量的小，不要转#role{}等大块数。大的数据可以通过读ets来实现
}).

%%% -------------------------------------------------------------------------------------------
%% 常用常量
%%% -------------------------------------------------------------------------------------------
-define(THREE_YEAR_SECOND, 31536000).               % 按照365天计算的三年秒数

-define(ONE_WEEK_SECOND, 604800).% 7 * 86400).		% 每周有多少秒
-define(ONE_DAY_SECOND, 86400).                     % 每天有多少秒
-define(ONE_HOUR_SECOND, 3600).                     % 每小时有多少秒
-define(ONE_MIN_SECOND, 60).                        % 每分钟有多少秒

-define(WEEK_S, ?ONE_WEEK_SECOND).       % 每周有多少秒 h5
-define(DAY_S, ?ONE_DAY_SECOND).         % 每天有多少秒 h5
-define(HOUR_S, ?ONE_HOUR_SECOND).       % 每小时有多少秒 h5
-define(MINU_S, ?ONE_MIN_SECOND).        % 每分钟有多少秒 h5

-define(WEEK_S(N), trunc(?WEEK_S * (N))).
-define(DAY_S(N), trunc(?DAY_S * (N))).
-define(HOUR_S(N), trunc(?HOUR_S * (N))).
-define(MINU_S(N), trunc(?MINU_S * (N))).

-define(WEEK_MS, 604800000).
-define(DAY_MS, 86400000).
-define(HOUR_MS, 3600000).
-define(MINU_MS, 60000).
-define(SEC_MS, 1000).

-define(WEEK_MS(N), trunc(?WEEK_MS * (N))).
-define(DAY_MS(N), trunc(?DAY_MS * (N))).
-define(HOUR_MS(N), trunc(?HOUR_MS * (N))).
-define(MINU_MS(N), trunc(?MINU_MS * (N))).
-define(SEC_MS(N), trunc(?SEC_MS * (N))).

-define(DATA_VALUE, 1).     % 值
-define(DATA_PERCENT, 2).   % 百分比

-define(MS_TO_SEC(N), (N div 1000)).

-define(UINT_32_MAX, 16#FFFFFFFF).                  % 无符号int32最大值
-define(INT_32_MAX, 16#7FFFFFFF).                   % 有符号int32最大值
-define(CALL_DEFAULT_TIMEOUT, 5000).                % call的默认超时时间

-define(TIME_TYPE_WEEK, 4).   % 周
-define(TIME_TYPE_DAY, 3).    % 天
-define(TIME_TYPE_HOUR, 2).   % 小时
-define(TIME_TYPE_MINU, 1).   % 分
-define(TIME_TYPE_SECS, 0).   % 秒

-define(BOOL_TRUE, 1).                              % 1表示true
-define(BOOL_FALSE, 0).                             % 0表示false
-define(no_data, no_data).

%% 几个用atom定义
-define(TRUE_INT, 1).
-define(FALSE_INT, 0).
-define(TRUE, true).
-define(FALSE, false).
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
-define(NONE, none).             % none
-define(none, none).             % none
-define(normal, normal).         % normal
-define(exit, exit).             % exit
-define(trap_exit, trap_exit).   % trap_exit
-define(inet_reply, inet_reply). % inet_reply
-define(func, func).             % func
-define(add, add).               % add
-define(del, del).               % del
-define(update, update).         % update
-define(not_found, not_found).   % not_found

-define(STRING_RANDS, ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]).

%% 服务器类型
-define(SERVER_TYPE_CENTER, 1).                                     % 中心服
-define(SERVER_TYPE_GAME, 2).                                       % 游戏服 / 大厅服
-define(SERVER_TYPE_PATH, 999).                                     % 寻路服

%% 服务器备份间隔（必须为偶数）
-define(ROLE_SAVE_INTERVAL, 10).                                    % 角色save间隔，10 min
-define(DB_SAVE_INTERNAL, ?ROLE_SAVE_INTERVAL * 2).                 % 数据库最小回档间隔，为角色dump间隔的两倍
-define(SAVE_RED, 10).                                              % 存储冗余，单位秒

%% 审核服
-define(CHECK_SERVER_XINXIN_IOS, xinxin_ios).                       % 欣欣ios审核服

%% 不需要state的gen_server
-define(STATE_NONE, none).

%% 超级随机功能标签
-define(FUN_TAG_RECRUIT, 1).                                        % 招募

%% 敏感字库
-define(BLOCK_WORLDS, block_words).

-define(DAY_SECONDS, 86400).

%% 永远
-define(INFINITY, 16#ffffffff).

%% 错误码
-define(C2SERR(R), throw({error, R})).
-define(IS_C2SERR(R), {error, R}).

%% 封装处理try ... catch
-define(TRY_CATCH(Fun),
    try
        (Fun)
    catch
        Class:Error ->
            ?ERROR("try_catch_exception:~w ~w", [Class, Error]),
            ok
    end).

-define(TRY_CATCH(Fun, ErrReason),
    try
        (Fun)
    catch
        _:ErrReason ->
            ?ERROR("try_catch_exception :~w", [ErrReason]),
            ok
    end).

-define(TRY(Code,Code2),try begin Code end catch _:_ -> begin Code2 end end).



-endif.

