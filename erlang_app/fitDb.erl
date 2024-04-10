-module(fitDb).
-include("fitDbStructure.hrl").
-include_lib("stdlib/include/qlc.hrl").

-compile([debug_info]).

-export([init_db/0, restart/0, check_for_schedules_ts/1, check_for_schedules_scheduleId/1, read_all_schedules/0, insert_schedule/2, delete_schedule/1, edit_schedule/2]).

% This function is called only once to create the table in the mnesiaDB with persistence
init_db() ->
    Node = [node()],
    mnesia:create_schema(Node),
    application:start(mnesia),
    mnesia:change_table_copy_type(schema, node(), disc_copies), % Persistence is activated on local disk (default is in RAM)
    mnesia:create_table(schedules, [{attributes, record_info(fields, schedules)}, {disc_copies, Node}]),
    {ok, []}.

% Called whenever the fitNotifier restarts
restart() ->
  application:start(mnesia),
  TableName = schedules,
  mnesia:wait_for_tables([TableName], 30000).

% Returns all schedules with equal or lower timestamp than the one specified
check_for_schedules_ts(Time) ->
  F = fun() ->
    Query = qlc:q([
        X || X <- mnesia:table(schedules),
              element(#schedules.timestamp, X) =< (Time - 1800000) % MODIFY TO CHECK FOR 30 BEFORE
    ]),
    qlc:eval(Query)
  end,
  mnesia:activity(transaction, F).

% Returns all schedules with equal or lower timestamp than the one specified
check_for_schedules_scheduleId(ScheduleId) ->
  F = fun() ->
    Query = qlc:q([
        X || X <- mnesia:table(schedules),
          element(2, element(#schedules.key, X)) == ScheduleId % It takes the 2nd element of the 1st element (key) of X
    ]),
    qlc:eval(Query)
  end,
  mnesia:activity(transaction, F).

% Reads all tuples and returns them as showed below
read_all_schedules() ->
  F = fun() ->
    mnesia:foldl(
      fun(#schedules{key = K, timestamp = T}, Acc) ->
        [{K, T} | Acc]
      end,
      [],
      schedules)
  end,
  mnesia:activity(transaction, F).
     
% Insert a record into the db 
insert_schedule(Key, Timestamp) ->
    F = fun() ->
      mnesia:write(#schedules{key = Key, timestamp = Timestamp})
    end,
    mnesia:activity(transaction, F).

% Deletes a tuple (schedule) from the db
delete_schedule(Key) ->
  %io:format("delete: ~p;~n", [Key]), % DEBUG
  F = fun() -> mnesia:delete({schedules, Key}) end,
  mnesia:activity(transaction, F).

% Edits the state of a tuple just removing it and adding it modified
edit_schedule(Key, Timestamp) ->
  %io:format("edit: ~p, ~p;~n", [Key, Timestamp]), % DEBUG
  delete_schedule(Key),
  insert_schedule(Key, Timestamp).