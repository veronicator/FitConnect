-module(fitNotifier).
-behaviour(gen_server).
-include("fitDbStructure.hrl").

-compile([debug_info]).

-import(fitDb, [init_db/0, restart/0, check_for_schedules_ts/1, check_for_schedules_scheduleId/1, read_all_schedules/0, insert_schedule/2, delete_schedule/1, edit_schedule/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, notify/0, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Called by the supervisor to start the fitNotifier
start_link() ->
    gen_server:start_link({local, fitNotifier}, ?MODULE, [], []).

% If it crashed then when need to resync it
% ROUND TO 5 OR 10 MIN
init([]) ->
    restart(),
    control_schedules(expired, erlang:system_time(millisecond)),
    {{_,_,_}, {_, Minute, Seconds}} = calendar:now_to_local_time(os:timestamp()),
    case Minute rem 10 of
        D when D >= 5 ->
            Rest = 9 - D;
        D ->
            Rest = 4 - D
    end,
    io:format("Rest: ~p, Seconds: ~p~n", [Rest, Seconds]),
    %RemainingMilliseconds = ((Rest * 60) + (60 - Seconds)) * 1000,
    %io:format("Milli remaining: ~p~n", [RemainingMilliseconds]),
    RemainingMilliseconds = 10000, % DEBUG
    timer:apply_after(RemainingMilliseconds, ?MODULE, notify, []),
    {ok, []}.

% Reads all schedules in the db
handle_call({read}, _From, Timers) ->
    Schedules = read_all_schedules(),
    io:format("Schedules: ~p~n", [Schedules]),
    {reply, {read, ok}, Timers};

% DEBUG
handle_call({match, Ts}, _From, Timers) ->
    control_schedules(expired, Ts),
    {reply, {read, ok}, Timers};

% Generates a new timer and add a tuple representing it to the persistent db
handle_call({insert, Username, ScheduleId, Timestamp}, _From, Timers) ->
    Key = {Username, ScheduleId},
    insert_schedule(Key, Timestamp),
    {reply, {added, ok}, Timers};

% Edits a timer that already existed and modifies the tuple representing it into the persistent db
handle_call({edit, _Username, ScheduleId, Timestamp}, _From, Timers) ->
    control_schedules(edited, {ScheduleId, Timestamp}),
    {reply, {edited, ok}, Timers};

% Removes timer and removes the tuple representing it from the persistent db
handle_call({delete, Username, ScheduleId, _Timestamp}, _From, Timers) ->
    Key = {Username, ScheduleId},
    delete_schedule(Key),
    {reply, {deleted, ok}, Timers};

handle_call(Request, _From, Timers) ->
    {ok, {error, "Unhandled Request", Request}, Timers}.

% Used to notify the users that the course is going to start sending a message to the fitMessanger
notify() ->
    io:format("Checking for schedules~n").%,
    %control_schedules(expired, erlang:system_time(millisecond)),
    %timer:apply_after(30000, ?MODULE, notify, []). % every 5 minutes a control is performed

handle_cast(_Request, Timers) ->
    {noreply, Timers}.
    
handle_info(_Info, Timers) ->
    {noreply, Timers}.

terminate(_Reason, _Timers) ->
    ok.

code_change(_OldVsn, Timers, _Extra) ->
    {ok, Timers}.

% Couple can be: expired -> Timestamp | edited -> {ScheduleId, Timestamp}
control_schedules(Mode, Element) ->
    case Mode of
        expired ->
            Schedules = check_for_schedules_ts(Element),
            if
                Schedules == [] ->
                    io:format("No schedules to send~n");
                true ->
                    [process_expired_schedule(Mode, Schedule) || Schedule <- Schedules]
            end;
        edited -> 
            {ScheduleId, Timestamp} = Element,
            Schedules = check_for_schedules_scheduleId(ScheduleId),
            if
                Schedules == [] ->
                    io:format("No schedules to send~n");
                true ->
                    [process_edited_schedule(Mode, Timestamp, Schedule) || Schedule <- Schedules]
            end
    end.

process_expired_schedule(Mode, {schedules, {Username, ScheduleId}, Timestamp}) ->
    Message = {schedule, Mode, Username, ScheduleId, Timestamp},
    io:format("Message to send: ~p~n", [Message]),
    Result = broadcast(Message),
    io:format("Result: ~p~n", [Result]),
    if
        Result == {ok} ->
            delete_schedule({Username, ScheduleId});
        true ->
            %io:format("Error sending notification~n"),
            ok % Do nothing
    end.

process_edited_schedule(Mode, NewTimestamp, {schedules, {Username, ScheduleId}, _Timestamp}) ->
    Message = {schedule, Mode, Username, ScheduleId, NewTimestamp},
    edit_schedule({Username, ScheduleId}, NewTimestamp),
    io:format("Message to send: ~p~n", [Message]),
    broadcast(Message).

broadcast(Msg) -> 
    case whereis(fitMessanger) of
        undefined ->
            io:format("Error: fitMessanger process not found.~n"),
            error;
        _ ->
            Reply = gen_server:call(fitMessanger, Msg),
            Reply   
    end.