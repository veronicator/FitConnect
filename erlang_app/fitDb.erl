-module(fitDb).
-include("fitDbStructure.hrl").

-compile([debug_info]).

-export([init_db/0, check_table_existence/0, recover_notifications/0, read_all_notifications/0, insert_notification/4, read_notification/1, delete_notification/1, edit_notification/4]).

% This function is called only once to create the table
init_db() ->
    Node = [node()],
    mnesia:create_schema(Node),
    application:start(mnesia),
    mnesia:change_table_copy_type(schema, node(), disc_copies), % Persistence is activated on local disk (default is in RAM)
    mnesia:create_table(notifications, [{attributes, record_info(fields, notifications)}, {disc_copies, Node}]),
    {ok, []}.

check_table_existence() ->
    TableName = notifications,  % Replace with the actual table name
    case table_exists(TableName) of
        true ->
            io:format("Table ~p exists.~n", [TableName]);
        false ->
            io:format("Table ~p does not exist.~n", [TableName])
    end.

recover_notifications() ->
  application:start(mnesia),
  TableName = notifications,
  mnesia:wait_for_tables([TableName], 30000),
    Records = mnesia:async_dirty(fun()-> qlc:e(mnesia:table(TableName)) end),
    Records.

    read_all_notifications() ->
      F = fun() ->
        mnesia:foldl(
          fun(#notifications{course = C, trainer = T, delay = D, requestTime = R}, Acc) ->
            [{C, T, D, R} | Acc]
          end,
          [],
          notifications)
      end,
      mnesia:activity(transaction, F).
     

% Insert a record into the db 
insert_notification(Course, Trainer, Delay, RequestTime) ->
    F = fun() ->
      mnesia:write(#notifications{course=Course, trainer=Trainer, delay=Delay, requestTime=RequestTime})
    end,
    mnesia:activity(transaction, F).

% Searches for the record with the corresponding key equal to Course number
read_notification(Course) ->
  F = fun() ->
    case mnesia:read({notifications, Course}) of
      [#notifications{trainer = T, delay= D, requestTime = R}] ->
        {Course, T, D, R};
    [] ->
      undefined
    end
  end,
  mnesia:activity(transaction, F).

delete_notification(Course) ->
  F = fun() -> mnesia:delete({notifications, Course}) end,
  mnesia:activity(transaction, F).

edit_notification(Course, Trainer, Delay, RequestTime) ->
    delete_notification(Course),
    insert_notification(Course, Trainer, Delay, RequestTime).

table_exists(TableName) ->
  case mnesia:table_info(TableName, disc_copies) of
      [server@a09ac34a7c62] ->
          true;  % Table exists
      undefined ->
          false   % Table does not exist
  end.