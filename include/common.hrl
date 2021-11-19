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
% 常用常量
-define(DATA_VALUE, 1).     % 值
-define(DATA_PERCENT, 2).   % 百分比

-define(UINT_32_MAX, 16#FFFFFFFF). % 无符号int32最大值
-define(INT_32_MAX, 16#7FFFFFFF).  % 有符号int32最大值

-define(TRUE_INT, 1).            % 1 表示true
-define(FALSE_INT, 0).           % 0 表示false

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
-define(local, local).           % local



%% 封装处理try ... catch
-define(TRY_SRV(FunCode, Mod, DoArgs, State),
    try
        case (FunCode) of
            ?ok -> {?noreply, {Mod, State}};
            {?ok, NewState} -> {?noreply, {Mod, NewState}};
            {?noreply, NewState} -> {?noreply, {Mod, NewState}};
            {?ok, Reply, NewState} -> {?reply, Reply, {Mod, NewState}};
            {?reply, Reply, NewState} -> {?reply, Reply, {Mod, NewState}};
            {?stop, Reason, NewState} -> {?stop, Reason, {Mod, NewState}};
            {?stop, Reason, Reply, NewState} -> {?stop, Reason, Reply, {Mod, NewState}};
            TryError ->
                ?ERROR("Mod:~w DoArgs:~w TryError:~w State:~w", [Mod, DoArgs, TryError, State]),
                {?noreply, {Mod, State}}
        end
    catch
        Class:TryError2 ->
            ?ERROR("Mod:~w DoArgs:~w Class:~w TryError:~w State:~w", [Mod, DoArgs, Class, TryError2, State])
    end).





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


-endif.