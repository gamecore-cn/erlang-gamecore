%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 19. 11月 2021 9:27
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__utils__hrl).
-define(__utils__hrl, true).

%%% define
%%% -------------------------------------------------------------------

%% @doc 时间相关
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

-define(SECOND_DIFF_SECONDS_0000_1900, 62167219200).   % 0000到1970年的秒数  0时区
-define(SECOND_EIGHT_ZONE_TIME_LINE, 62167248000).     % 0000到1970年的秒数  东8区
-define(SECOND_YEAR_THREE, 31536000).                  % 按照365天计算的三年秒数

-define(TIME_TYPE_WEEK, 4).   % 周
-define(TIME_TYPE_DAY, 3).    % 天
-define(TIME_TYPE_HOUR, 2).   % 小时
-define(TIME_TYPE_MINU, 1).   % 分
-define(TIME_TYPE_SECS, 0).   % 秒

-define(MS_TO_SECOND(N), (N div 1000)).

%% @doc echeck
-define(IF(Expr, True, False), case Expr of true -> True; false -> False end).
-define(IF_NOT_EMPTY(Expr, True, False), case Expr of [_ | _] -> True; _ -> False end).
-define(IF_CATCH(Expr, True, False), case Expr of true -> True; false -> erlang:throw(False) end).

-define(IF_REAL, fun echeck:real/2).                      % true or false，true返回ok
-define(IF_ALL_EQUAL, fun echeck:all_equal/3).            % =:=
-define(IF_NOT, fun echeck:negative/3).                   % =/=
-define(IF_EQUAL, fun echeck:equal/3).                    % ==
-define(IF_EQUAL_BIGGER, fun echeck:bigger_equal/3).      % >=
-define(IF_EQUAL_LESS, fun echeck:less_equal/3).          % =<
-define(IF_BIGGER, fun echeck:bigger/3).                  % >
-define(IF_LESS, fun echeck:less/3).                      % <



-endif.