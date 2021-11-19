%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 12:18
%%%-------------------------------------------------------------------
-author("xiayiping").
-ifndef(__mvc__hrl).
-define(__mvc__hrl, true).

%%% define
%%% -------------------------------------------------------------------


%%% record
%%% -------------------------------------------------------------------
%% 通知 notification
-record(n, {
    name :: atom()      % 名称
    , type :: integer() % 类型
    , body :: term()    % 通知内容
}).

%% 事件监听的信号 signal
-record(s, {
    id :: atom()              % 信号id,同协议号的区间并要求在对应模块*.hrl里用常量定好  类型:unit32()
    , target_id :: integer()  % 目标模块id，同模块协议区间 在common.hrl常量定义好  类型:unit32()
    , data :: term()          % 数据，请尽量的小，不要转#role{}等大块数。大的数据可以通过读ets来实现
}).

-endif.