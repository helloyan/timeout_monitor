%%%-------------------------------------------------------------------
%% @doc timeout_monitor public API
%% @end
%%%-------------------------------------------------------------------

-module(timeout_monitor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    timeout_monitor_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
