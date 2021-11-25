%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 14:35
%%%-------------------------------------------------------------------
-module(etimer).
-author("xiayiping").
-include("common.hrl").
-include("utils.hrl").

%% API
-export([now_second/0, now_millisec/0, now_microsec/0, now_date/0, now_time/0, sleep/1, sleep/3, now_second_local/0, second_to_datetime/1, datetime_to_second/1, day_interval/2, is_same_day/2, is_same_day/3, diff_date_plus/2, diff_date_plus/3, today_second/3, the_week_day_second/4, next_week_day_second/4, now_week/0, second_to_week/1, add_day/1, add_day/2, sub_day/1, sub_day/2, get_next_day_zero_time_len/1, get_zero_oclock_of_timestamp/0, get_zero_oclock_of_timestamp/1, now_to_iso_string/0, seconds2localtime/1]).


%%% -------------------------------------------------------------
%%% 时间函数
%%% -------------------------------------------------------------
%% @doc 当前秒
now_second() ->
    erlang:system_time(second).

%% @doc 当前毫秒
now_millisec() ->
    erlang:system_time(millisecond).

%% @doc 当前微秒
now_microsec() ->
    erlang:system_time(microsecond).

%% @doc
now_date() ->
    {Date, _} = second_to_datetime(now_second()),
    Date.

%% @doc
now_time() ->
    {_, Time} = second_to_datetime(now_second()),
    Time.


%% @doc 暂停(毫秒)
-spec sleep(Millisecond :: integer()) -> boolean().
sleep(Millisecond) ->
    receive
    after Millisecond -> ?true
    end.

%% @doc
-spec sleep(Millisecond :: integer(), Fun :: function(), Args :: list()) -> any().
sleep(Millisecond, Fun, Args) when is_function(Fun) andalso is_list(Args) ->
    receive
    after Millisecond -> erlang:apply(Fun, Args)
    end.

%% @doc
now_second_local() ->
    {M, S, _MicroSecs} = os:timestamp(),
    1000000 * M + S.

%% @doc 时间戳转换成日期 {{年, 月, 日}, {时, 分, 秒}}
second_to_datetime(Now) ->
    MS = Now div 1000 div 1000,
    S = Now rem (1000 * 1000),
    calendar:now_to_local_time({MS, S, 0}).

%% @doc
datetime_to_second(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?SECOND_EIGHT_ZONE_TIME_LINE.

%% @doc 计算2个时间戳的日期间隔
day_interval(Time1, Time2) when is_integer(Time1) andalso is_integer(Time2) ->
    Interval = abs(Time1 - Time2),
    erlang:trunc(Interval / ?SECOND_DAY).

%% @doc 2个时间戳是否在同一天
is_same_day(Time1, Time2) ->
    {Date1, _} = second_to_datetime(Time1),
    {Date2, _} = second_to_datetime(Time2),
    Date1 =:= Date2.

%% @doc 基于Hour作为分界点，2个时间戳是否同一天
is_same_day(Time1, Time2, Hour) ->
    Diff = Hour * ?SECOND_HOUR,
    is_same_day(Time1 - Diff, Time2 - Diff).

%% @doc 比较两个时间戳天数差
diff_date_plus(Time1, Time2) ->
    {Date1, _} = second_to_datetime(Time1),
    {Date2, _} = second_to_datetime(Time2),
    calendar:date_to_gregorian_days(Date1) - calendar:date_to_gregorian_days(Date2).

%% @doc 比较两个时间戳天数差,以小时区分
diff_date_plus(Time1, Time2, Hour) ->
    {Date1, {Hour1, _, _}} = second_to_datetime(Time1),
    Data11 =
        case Hour1 >= Hour of
            true ->
                Date1;
            false ->
                sub_day(Date1, 1)
        end,
    {Date2, {Hour2, _, _}} = second_to_datetime(Time2),
    Date22 =
        case Hour2 >= Hour of
            true ->
                Date2;
            false ->
                sub_day(Date2, 1)
        end,
    calendar:date_to_gregorian_days(Data11) - calendar:date_to_gregorian_days(Date22).

%% @doc 计算今天 时:分:秒 的秒级时间戳
today_second(Hour, Minute, Second) ->
    datetime_to_second({date(), {Hour, Minute, Second}}).

%% @doc 计算本周 星期几:时:分:秒 的秒级时间戳
the_week_day_second(Week, Hour, Minute, Second) ->
    today_second(Hour, Minute, Second) + (Week - calendar:day_of_the_week(date())) * ?SECOND_DAY.

%% @doc 计算下周 星期几:时:分:秒 的秒级时间戳
next_week_day_second(Week, Hour, Minute, Second) ->
    ThisWeekEnd = the_week_day_second(7, 23, 59, 59) + 1,
    ThisWeekEnd + ((Week - 1) * ?SECOND_DAY) + (Hour * ?SECOND_HOUR) + (Minute * ?SECOND_MINUTES) + Second.

%% @doc 今天是周几？
now_week() ->
    calendar:day_of_the_week(date()).

%% @doc 时间戳是周几
second_to_week(NowSecond) ->
    {{Y, Mon, D}, {_H, _Min, _S}} = second_to_datetime(NowSecond),
    calendar:day_of_the_week({Y, Mon, D}).

%% @doc 加天数 返回date {year, month, day}
add_day(AddNum) ->
    add_day(now_date(), AddNum).

add_day(Date, AddNum) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + AddNum).

%% @doc 减天数 返回date {year, month, day}
sub_day(SubNum) ->
    sub_day(now_date(), SubNum).

sub_day(Date, SubNum) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - SubNum).

%% @doc 根据传入时间计算距离下一天零点的时间差
get_next_day_zero_time_len(Time) ->
    {{NowYear, NowMonth, NowLoginDay}, _} = second_to_datetime(Time),
    NowDayZeroTime = datetime_to_second({{NowYear, NowMonth, NowLoginDay}, {0, 0, 0}}),
    NextDayZeroTime = NowDayZeroTime + (60 * 60 * 24),
    NextDayZeroTime - Time.

%% @doc 获取当天0点的时间戳
get_zero_oclock_of_timestamp() ->
    NowTime = now_second(),
    get_zero_oclock_of_timestamp(NowTime).

%% @doc 获取指定时间当天0点的时间戳
get_zero_oclock_of_timestamp(Time) ->
    {Date, _} = second_to_datetime(Time),
    datetime_to_second({Date, {0, 0, 0}}).

%% 格式化当前时间
%% @doc YYYY-MM-DD HH:mm:SS
now_to_iso_string() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second]
        )
    ).

%% @doc 根据1970年以来的秒数获得日期
seconds2localtime(Seconds) ->
    Seconds2 = eutil:to_integer(Seconds),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds2 + ?SECOND_DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).
