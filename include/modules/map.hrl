%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 23. 11月 2021 16:55
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__map__hrl).
-define(__map__hrl, true).

%%% define
%%% -------------------------------------------------------------------
%% !!!下面尽量按模板大小来分，避免系统自带城池跨越两个场景
-define(MAP_TILE_LEN, 8).                                               % 九宫广播，单个宫格的路点数量
-define(MAP_TEMPLATE_SIZE, 49).                                         % 地图模板大小
-define(MAP_SLICE_LEN, (?MAP_TILE_LEN * ?MAP_TEMPLATE_SIZE)).           % 地图分割大小，必须是单个宫格的整数倍，否则一个宫格会跨两个地图
-define(MAP_INDEX_MAX, erlang:ceil(?MAP_WIDTHS / ?MAP_SLICE_LEN)).

-define(MAP_WIDTHS, 3969).                              % 地图宽度
-define(MAP_HEIGHT, 3969).                              % 地图高度
-define(MAP_PREFIX, 10000).                             % 大格子{X,Y}转Index前缀  ?SMALL_POS_PREFIX

-define(UNIT_SIZE, 49).
-define(UNIT_NUM_X, (((?MAP_WIDTHS - 1) div ?UNIT_SIZE) + 1)).

-define(RES_RECTANGLE_SIZE_MIN, 4).
-define(RES_RECTANGLE_SIZE_MAX, 16).

%% 位置是否相同  s_pos_to_b_pos_if
-define(MAP_POS_EQUAL(PosX, PosY, PosX2, PosY2), {PosX, PosY} =:= {PosX2, PosY2}).

-define(MILLI_LOOP_INTERVAL, 250).                      % 地图帧循环间隔
-define(SECOND_LOOP_INTERVAL, 1000).                    % 地图秒循环间隔

-define(MAP_SEND_SRV_NUM, 32).                          % 发送进程数量
-define(TILE_DECIMAL_PREFIX, 100000).                   % 九宫十进制位的进位数


-endif.