%% common_test suite for timeout_monitor

-module(timeout_monitor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). % Eunit macros for convenience

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 200}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%%      NB: By default, we export all 1-arity user defined functions
%%--------------------------------------------------------------------
all() ->
  [test_timeout_monitor, test_timeout_monitor_2].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
  Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
  Config.

%% 常规测试
test_timeout_monitor(_Config) ->
  ct:log("~p", [self()]),
  application:ensure_all_started(timeout_monitor),
  {ok, Pid} = timeout_monitor:start_monitor(self(), test, 5000),
  timer:sleep(5500),
  {messages, [{test, 1}]} = erlang:process_info(self(), messages),
  timer:sleep(5500),
  {messages, [{test, 1}, {test, 2}]} = erlang:process_info(self(), messages),
  c:flush(),
  erlang:send(Pid, {reset, self(), test}),
  timer:sleep(5500),
  {messages, [{test, 1}]} = erlang:process_info(self(), messages),
  timer:sleep(5500),
  {messages, [{test, 1}, {test, 2}]} = erlang:process_info(self(), messages),
  c:flush(),
  erlang:send(Pid, {reset, self(), test}),
  timer:sleep(5500),
  {messages, [{test, 1}]} = erlang:process_info(self(), messages),
  timer:sleep(5500),
  {messages, [{test, 1}, {test, 2}]} = erlang:process_info(self(), messages),
  c:flush(),
  ok = timeout_monitor:stop_monitor(Pid),
  ok.

%% 接收异常消息测试
test_timeout_monitor_2(_Config) ->
  ct:log("~p", [self()]),
  {ok, Pid} = timeout_monitor:start_monitor(self(), test, 5000),
  timer:sleep(5500),
  {messages, [{test, 1}]} = erlang:process_info(self(), messages),
  erlang:send(Pid, test),
  timer:sleep(5500),
  {messages, [{test, 1}, {test, 2}]} = erlang:process_info(self(), messages),
  erlang:send(Pid, test1),
  timer:sleep(5500),
  {messages, [{test, 1}, {test, 2}, {test, 3}]} = erlang:process_info(self(), messages),
  c:flush(),
  gen_server:cast(Pid, test2),
  timer:sleep(5500),
  {messages, [{test, 4}]} = erlang:process_info(self(), messages),
  ignore = gen_server:call(Pid, test3),
  timer:sleep(5500),
  {messages, [{test, 4}, {test, 5}]} = erlang:process_info(self(), messages),
  c:flush(),
  ok = timeout_monitor:stop_monitor(Pid),
  ok.