%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 15:08
%%%-------------------------------------------------------------------
-module(utils_edate).
-author("xiayiping").

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(DEV_ONLY).
-include_lib("eunit/include/eunit.hrl").


-define(DATE, {{2001,3,10},{17,16,17}}).
-define(DATE_MS, {{2001,3,10},{17,16,17,123456}}).
-define(DATE_NOON, {{2001,3,10},{12,0,0}}).
-define(DATE_MIDNIGHT, {{2001,3,10},{0,0,0}}).
-define(ISO, "o \\WW").

basic_format_test_() ->
    [
        ?_assertEqual(edate:format("F j, Y, g:i a",?DATE), "March 10, 2001, 5:16 pm"),
        ?_assertEqual(format("F jS, Y, g:i a",?DATE), "March 10th, 2001, 5:16 pm"),
        ?_assertEqual(format("F jS",{{2011,3,21},{0,0,0}}), "March 21st"),
        ?_assertEqual(format("F jS",{{2011,3,22},{0,0,0}}), "March 22nd"),
        ?_assertEqual(format("F jS",{{2011,3,23},{0,0,0}}), "March 23rd"),
        ?_assertEqual(format("F jS",{{2011,3,31},{0,0,0}}), "March 31st"),
        ?_assertEqual(format("m.d.y",?DATE), "03.10.01"),
        ?_assertEqual(format("j, n, Y",?DATE), "10, 3, 2001"),
        ?_assertEqual(format("Ymd",?DATE), "20010310"),
        ?_assertEqual(format("H:i:s",?DATE), "17:16:17"),
        ?_assertEqual(format("z",?DATE), "68"),
        ?_assertEqual(format("D M j G:i:s Y",?DATE), "Sat Mar 10 17:16:17 2001"),
        ?_assertEqual(format("ga",?DATE_NOON), "12pm"),
        ?_assertEqual(format("gA",?DATE_NOON), "12PM"),
        ?_assertEqual(format("ga",?DATE_MIDNIGHT), "12am"),
        ?_assertEqual(format("gA",?DATE_MIDNIGHT), "12AM"),

        ?_assertEqual(format("h-i-s, j-m-y, it is w Day",?DATE),
            "05-16-17, 10-03-01, 1631 1617 6 Satpm01"),
        ?_assertEqual(format("\\i\\t \\i\\s \\t\\h\\e\\ jS \\d\\a\\y.",?DATE),
            "it is the 10th day."),
        ?_assertEqual(format("H:m:s \\m \\i\\s \\m\\o\\n\\t\\h",?DATE),
            "17:03:17 m is month")
    ].

basic_parse_test_() ->
    [
        ?_assertEqual({{2008,8,22}, {17,16,17}},
            parse("22nd of August 2008", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,0,0}},
            parse("22-Aug-2008 6 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("22-Aug-2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,12}},
            parse("22-Aug-2008 6:35:12 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,0,0}},
            parse("August/22/2008 6 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("August/22/2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("22 August 2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,0,0}},
            parse("22 Aug 2008 6AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("22 Aug 2008 6:35AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("22 Aug 2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,0,0}},
            parse("22 Aug 2008 6", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("22 Aug 2008 6:35", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,35,0}},
            parse("22 Aug 2008 6:35 PM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,0,0}},
            parse("22 Aug 2008 6 PM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,0,0}},
            parse("Aug 22, 2008 6 PM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,0,0}},
            parse("August 22nd, 2008 6:00 PM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,15,15}},
            parse("August 22nd 2008, 6:15:15pm", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,15,15}},
            parse("August 22nd, 2008, 6:15:15pm", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,15,0}},
            parse("Aug 22nd 2008, 18:15", ?DATE)),
        ?_assertEqual({{2008,8,2}, {17,16,17}},
            parse("2nd of August 2008", ?DATE)),
        ?_assertEqual({{2008,8,2}, {17,16,17}},
            parse("August 2nd, 2008", ?DATE)),
        ?_assertEqual({{2008,8,2}, {17,16,17}},
            parse("2nd  August, 2008", ?DATE)),
        ?_assertEqual({{2008,8,2}, {17,16,17}},
            parse("2008 August 2nd", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,0,0}},
            parse("2-Aug-2008 6 AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,35,0}},
            parse("2-Aug-2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,35,12}},
            parse("2-Aug-2008 6:35:12 AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,0,0}},
            parse("August/2/2008 6 AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,35,0}},
            parse("August/2/2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,35,0}},
            parse("2 August 2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,0,0}},
            parse("2 Aug 2008 6AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,35,0}},
            parse("2 Aug 2008 6:35AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,35,0}},
            parse("2 Aug 2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,0,0}},
            parse("2 Aug 2008 6", ?DATE)),
        ?_assertEqual({{2008,8,2}, {6,35,0}},
            parse("2 Aug 2008 6:35", ?DATE)),
        ?_assertEqual({{2008,8,2}, {18,35,0}},
            parse("2 Aug 2008 6:35 PM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {18,0,0}},
            parse("2 Aug 2008 6 PM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {18,0,0}},
            parse("Aug 2, 2008 6 PM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {18,0,0}},
            parse("August 2nd, 2008 6:00 PM", ?DATE)),
        ?_assertEqual({{2008,8,2}, {18,15,15}},
            parse("August 2nd 2008, 6:15:15pm", ?DATE)),
        ?_assertEqual({{2008,8,2}, {18,15,15}},
            parse("August 2nd, 2008, 6:15:15pm", ?DATE)),
        ?_assertEqual({{2008,8,2}, {18,15,0}},
            parse("Aug 2nd 2008, 18:15", ?DATE)),
        ?_assertEqual({{2012,12,10}, {0,0,0}},
            parse("Dec 10th, 2012, 12:00 AM", ?DATE)),
        ?_assertEqual({{2012,12,10}, {0,0,0}},
            parse("10 Dec 2012 12:00 AM", ?DATE)),
        ?_assertEqual({{2001,3,10}, {11,15,0}},
            parse("11:15", ?DATE)),
        ?_assertEqual({{2001,3,10}, {1,15,0}},
            parse("1:15", ?DATE)),
        ?_assertEqual({{2001,3,10}, {1,15,0}},
            parse("1:15 am", ?DATE)),
        ?_assertEqual({{2001,3,10}, {0,15,0}},
            parse("12:15 am", ?DATE)),
        ?_assertEqual({{2001,3,10}, {12,15,0}},
            parse("12:15 pm", ?DATE)),
        ?_assertEqual({{2001,3,10}, {3,45,39}},
            parse("3:45:39", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("23-4-1963", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("23-april-1963", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("23-apr-1963", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("4/23/1963", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("april/23/1963", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("apr/23/1963", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("1963/4/23", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("1963/april/23", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("1963/apr/23", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("1963-4-23", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("1963-4-23", ?DATE)),
        ?_assertEqual({{1963,4,23}, {17,16,17}},
            parse("1963-apr-23", ?DATE)),
        ?_assertThrow({?MODULE, {bad_date, "23/ap/195"}},
            parse("23/ap/195", ?DATE)),
        ?_assertEqual({{2001,3,10}, {6,45,0}},
            parse("6:45 am", ?DATE)),
        ?_assertEqual({{2001,3,10}, {18,45,0}},
            parse("6:45 PM", ?DATE)),
        ?_assertEqual({{2001,3,10}, {18,45,0}},
            parse("6:45 PM ", ?DATE))
    ].

parse_with_days_test_() ->
    [
        ?_assertEqual({{2008,8,22}, {17,16,17}},
            parse("Sat 22nd of August 2008", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("Sat, 22-Aug-2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,12}},
            parse("Sunday 22-Aug-2008 6:35:12 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("Sun 22-Aug-2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("THURSDAY, 22-August-2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,0,0}},
            parse("THURSDAY, 22-August-2008 6 pM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("THU 22 August 2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("FRi 22 Aug 2008 6:35AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,0,0}},
            parse("FRi 22 Aug 2008 6AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("Wednesday 22 Aug 2008 6:35 AM", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,35,0}},
            parse("Monday 22 Aug 2008 6:35", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,0,0}},
            parse("Monday 22 Aug 2008 6", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,0,0}},
            parse("Monday 22 Aug 2008 6p", ?DATE)),
        ?_assertEqual({{2008,8,22}, {6,0,0}},
            parse("Monday 22 Aug 2008 6a", ?DATE)),
        ?_assertEqual({{2008,8,22}, {18,35,0}},
            parse("Mon, 22 Aug 2008 6:35 PM", ?DATE))
    ].

parse_with_TZ_test_() ->
    [
        ?_assertEqual({{2008,8,22}, {17,16,17}},
            parse("Sat 22nd of August 2008 GMT", ?DATE)),
        ?_assertEqual({{2008,8,22}, {17,16,17}},
            parse("Sat 22nd of August 2008 UTC", ?DATE)),
        ?_assertEqual({{2008,8,22}, {17,16,17}},
            parse("Sat 22nd of August 2008 DST", ?DATE))
    ].

iso_test_() ->
    [
        ?_assertEqual("2004 W53",format(?ISO,{{2005,1,1}, {1,1,1}})),
        ?_assertEqual("2004 W53",format(?ISO,{{2005,1,2}, {1,1,1}})),
        ?_assertEqual("2005 W52",format(?ISO,{{2005,12,31},{1,1,1}})),
        ?_assertEqual("2007 W01",format(?ISO,{{2007,1,1}, {1,1,1}})),
        ?_assertEqual("2007 W52",format(?ISO,{{2007,12,30},{1,1,1}})),
        ?_assertEqual("2008 W01",format(?ISO,{{2007,12,31},{1,1,1}})),
        ?_assertEqual("2008 W01",format(?ISO,{{2008,1,1}, {1,1,1}})),
        ?_assertEqual("2009 W01",format(?ISO,{{2008,12,29},{1,1,1}})),
        ?_assertEqual("2009 W01",format(?ISO,{{2008,12,31},{1,1,1}})),
        ?_assertEqual("2009 W01",format(?ISO,{{2009,1,1}, {1,1,1}})),
        ?_assertEqual("2009 W53",format(?ISO,{{2009,12,31},{1,1,1}})),
        ?_assertEqual("2009 W53",format(?ISO,{{2010,1,3}, {1,1,1}}))
    ].

ms_test_() ->
    Now=now(),
    [
        ?_assertEqual({{2012,12,12}, {12,12,12,1234}}, parse("2012-12-12T12:12:12.1234")),
        ?_assertEqual(format("H:m:s.f \\m \\i\\s \\m\\o\\n\\t\\h",?DATE_MS),
            "17:03:17.123456 m is month"),
        ?_assertEqual(format("Y-m-d\\TH:i:s.f",?DATE_MS),
            "2001-03-10T17:16:17.123456"),
        ?_assertEqual(format("Y-m-d\\TH:i:s.f",nparse("2001-03-10T05:16:17.123456")),
            "2001-03-10T05:16:17.123456"),
        ?_assertEqual(format("Y-m-d\\TH:i:s.f",nparse("2001-03-10T05:16:17.123456")),
            "2001-03-10T05:16:17.123456"),
        ?_assertEqual(format("Y-m-d\\TH:i:s.f",nparse("2001-03-10T15:16:17.123456")),
            "2001-03-10T15:16:17.123456"),
        ?_assertEqual(Now, nparse(format("Y-m-d\\TH:i:s.f", Now)))
    ].

zulu_test_() ->
    [
        ?_assertEqual(format("Y-m-d\\TH:i:sZ",nparse("2001-03-10T15:16:17.123456")),
            "2001-03-10T15:16:17Z"),
        ?_assertEqual(format("Y-m-d\\TH:i:s",nparse("2001-03-10T15:16:17Z")),
            "2001-03-10T15:16:17"),
        ?_assertEqual(format("Y-m-d\\TH:i:s",nparse("2001-03-10T15:16:17+04")),
            "2001-03-10T11:16:17"),
        ?_assertEqual(format("Y-m-d\\TH:i:s",nparse("2001-03-10T15:16:17+04:00")),
            "2001-03-10T11:16:17"),
        ?_assertEqual(format("Y-m-d\\TH:i:s",nparse("2001-03-10T15:16:17-04")),
            "2001-03-10T19:16:17"),
        ?_assertEqual(format("Y-m-d\\TH:i:s",nparse("2001-03-10T15:16:17-04:00")),
            "2001-03-10T19:16:17")
    ].
-endif.
