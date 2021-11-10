%%%-------------------------------------------------------------------
%%% @author xiayiping@qq.com
%%% @copyright (C) 2021, Gamecore.cn MVC
%%% @doc
%%%
%%% @end
%%% Created : 09. 11月 2021 17:35
%%%-------------------------------------------------------------------
-module(echeck).
-author("xiayiping").

%% API
-export([real/2, all_equal/3, negative/3, equal/3, bigger_equal/3, less_equal/3, bigger/3, less/3]).

%% 逻辑验证
real(Expr, ErrorNo) -> case Expr of true -> ok; false -> ErrorNo end.
all_equal(A, B, ErrorNo) -> case A =:= B of true -> ok; false -> ErrorNo end.
negative(A, B, ErrorNo) -> case A =/= B of true -> ok; false -> ErrorNo end.
equal(A, B, ErrorNo) -> case A == B of true -> ok; false -> ErrorNo end.
bigger_equal(A, B, ErrorNo) -> case A >= B of true -> ok; false -> ErrorNo end.
less_equal(A, B, ErrorNo) -> case A =< B of true -> ok; false -> ErrorNo end.
bigger(A, B, ErrorNo) -> case A > B of true -> ok; false -> ErrorNo end.
less(A, B, ErrorNo) -> case A < B of true -> ok; false -> ErrorNo end.