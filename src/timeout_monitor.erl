%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% 外部接口模块，对外服务应当均通过该模块
%%%
%%% @end
%%% Created : 02. 三月 2017 19:04
%%%-------------------------------------------------------------------
-module(timeout_monitor).
-author("james").

%% API
-export([start_monitor/3, stop_monitor/1]).

-define(SUP_SERVER, timeout_monitor_sup).


%%%===================================================================
%%% @doc
%%% 启动 monitor server
%%% 仅在异常退出时supervisor会重启该server，当被监控的进程不存在时监控进程需结束
%%%
%%% Pid : 接收超时消息的进程
%%% MonitorType ： 标识该超时消息类型，同一进程的标识符应该唯一
%%% Interval : 超时间隔，ms
%%% @end
%%%===================================================================

-spec(start_monitor(Pid :: pid(), MonitorType :: atom(),
    Interval :: integer()) ->
  {ok, ChildPid :: pid()}).
start_monitor(Pid, MonitorType, Interval) ->
  supervisor:start_child(?SUP_SERVER, [Pid, MonitorType, Interval]).

%%%===================================================================
%%% @doc
%%% 停止 monitor server
%%% 应该仅有被监控进程发起停止监控进程的请求
%%%
%%% Pid : 监控进程pid
%%% @end
%%%===================================================================

-spec(stop_monitor(Pid :: pid()) ->
  ok | {error, Error :: term()}).
stop_monitor(Pid) ->
  supervisor:terminate_child(?SUP_SERVER, Pid).