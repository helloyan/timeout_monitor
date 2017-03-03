%%%-------------------------------------------------------------------
%% @doc timeout_monitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(timeout_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%% 所有子进程均动态生成
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  MonitorSpec = {
    monitor_server,
    {monitor_server, start_link, []},
    transient,
    2000,
    worker,
    [monitor_server]},
  Specs = [MonitorSpec],
  {ok, {{simple_one_for_one, 5, 10}, Specs}}.

%%====================================================================
%% Internal functions
%%====================================================================