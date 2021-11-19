%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 19. 11月 2021 14:17
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__gateway__hrl).
-define(__gateway__hrl, true).
-include("common.hrl").

%%% define
%%% -------------------------------------------------------------------
-define(TRY_FUN_SRV(State, Fun, Args),
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
                    ?ERROR("FUN
Funs :~w
Args :~w
Error:~p
State:~w", [Fun, Args, TryError, State]),
                    {?noreply, State}
            end
    end).




-endif.