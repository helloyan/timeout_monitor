%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% 超时监控服务器
%%% 流程：生成带超时的进程->超时时间到未收到被监控进程消息->发送超时消息给被监控进程->+1继续等待下一次超时
%%%      生成带超时的进程->超时时间未到收到被监控进程消息->清零继续等待下一次超时
%%% @end
%%% Created : 02. 三月 2017 19:40
%%%-------------------------------------------------------------------
-module(monitor_server).
-author("james").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([get_timestamp/0]).

-define(SERVER, ?MODULE).

-record(state, {
  pid, % 监控进程pid
  type, % 超时类型
  interval, % 超时间隔
  count, % 超时计次
  next_timestamp % 下一个超时消息的时间点，避免收到外部干扰消息导致循环失效
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Pid :: pid(), MonitorType :: atom(),
    Interval :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Pid, MonitorType, Interval) ->
  gen_server:start_link({local, list_to_atom(lists:flatten(io_lib:format("~p-~p", [Pid, MonitorType])))}, ?MODULE, [Pid, MonitorType, Interval], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Pid, MonitorType, Interval]) ->
  process_flag(trap_exit, true),
  {ok, #state{pid = Pid, type = MonitorType, interval = Interval, count = 0, next_timestamp = get_timestamp() + Interval}, Interval}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, #state{count = Count, interval = Interval, next_timestamp = Stamp} = State) ->
  case get_timestamp() of
    %% 在下次超时消息之前到来，补齐剩下的超时时间
    Time when Time < Stamp ->
      {reply, ignore, State, Stamp - get_timestamp()};
    %% 剩余应该就是等于了，那就按照收到超时消息的方式来，但是保守起见，不发消息给被监控进程
    _ -> {reply, ignore, State#state{count = Count + 1, next_timestamp = get_timestamp() + Interval}, Interval}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, #state{count = Count, interval = Interval, next_timestamp = Stamp} = State) ->
  case get_timestamp() of
    %% 在下次超时消息之前到来，补齐剩下的超时时间
    Time when Time < Stamp ->
      {noreply, State, Stamp - get_timestamp()};

    %% 剩余应该就是等于了，那就按照收到超时消息的方式来，但是保守起见，不发消息给被监控进程
    _ -> {noreply, State#state{count = Count + 1, next_timestamp = get_timestamp() + Interval}, Interval}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

%% 收到设定的超时信息
handle_info(timeout, #state{pid = Pid, type = Type, count = Count, interval = Interval} = State) ->
  case is_process_alive(Pid) of
    true ->
      erlang:send(Pid, {Type, Count + 1}),
      {noreply, State#state{count = Count + 1, next_timestamp = get_timestamp() + Interval}, Interval};
    false ->
      %% 被监控进程已退出
      {stop, normal, state}
  end;

%% 收到重设超时信息
handle_info({reset, Pid, Type}, #state{pid = Pid, type = Type, interval = Interval} = State) ->
  case is_process_alive(Pid) of
    true ->
      {noreply, State#state{count = 0, next_timestamp = get_timestamp() + Interval}, Interval};
    false ->
      %% 被监控进程已退出
      {stop, normal, state}
  end;

handle_info(_Info, #state{count = Count, interval = Interval, next_timestamp = Stamp} = State) ->
  case get_timestamp() of
    %% 在下次超时消息之前到来，补齐剩下的超时时间
    Time when Time < Stamp ->
      {noreply, State, Stamp - get_timestamp()};

    %% 剩余应该就是等于了，那就按照收到超时消息的方式来，但是保守起见，不发消息给被监控进程
    _ -> {noreply, State#state{count = Count + 1, next_timestamp = get_timestamp() + Interval}, Interval}
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_timestamp() -> integer().
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).