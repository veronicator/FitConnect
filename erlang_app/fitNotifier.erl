-module(fitNotifier).
-behaviour(gen_server).
-include("fitDbStructure.hrl").

-compile([debug_info]).

-import(fitDb, [init_db/0, start/0, recover_notifications/0, insert_notification/4, delete_notification/1, edit_notification/4]).

-export([start_link/0]).
-export([init/1, handle_call/3, notify_wrapper/2, update_wrapper/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Called by the supervisor to start the fitNotifier
start_link() ->
    gen_server:start_link({local, fitNotifier}, ?MODULE, [], []).

% Structure of the notifications in mnesiaDB -> record(notifications, {course, trainer, delay, requestTime})
% If it crashed then it gets all the states from the persistent db, if it is the first time is just empty
init([]) ->
    init_db(),
    Recover = recover_notifications(),
    NewTimers = lists:map(fun({notifications, Course, Trainer, Delay, RequestTime}) ->
       CurrentTime = erlang:system_time(millisecond),
       NewDelay = Delay - (CurrentTime - RequestTime),
       io:format("The new delay is ~p~n", [NewDelay]),
       if
           NewDelay >= 0 ->
               TimerRef = timer:apply_after(NewDelay, ?MODULE, notify_wrapper, [Course, Trainer]), % Generates timer
               {TimerRef, Course, Trainer};
           true ->
               io:format("Delay is negative, skipping timer~n"),
               delete_notification(Course)
       end
   end, Recover),
   FilteredTimers = lists:filter(fun(X) -> X /= ok end, NewTimers), % Removes all empty timers that elapsed while server was down
   io:format("States: ~p~n", [FilteredTimers]),
   {ok, FilteredTimers}.

% Generates a new timer and add a tuple representing it to the persistent db
handle_call({insert, Course, Trainer, Delay}, _From, Timers) ->
    TimerRef = timer:apply_after(Delay, ?MODULE, notify_wrapper, [Course, Trainer]),
    io:format("TimerRef generated ~p~n", [TimerRef]),
    NewTimer = {TimerRef, Course, Trainer},
    NewTimers = [NewTimer | Timers],
    io:format("Timers active ~p~n", [NewTimers]),
    % Update the mnesiaDB
    Time = erlang:system_time(millisecond),
    insert_notification(Course, Trainer, Delay, Time),
    {reply, {added, ok}, NewTimers};

% Edits a timer that already existed and modifies the tuple representing it into the persistent db
handle_call({edit, Course, Trainer, NewDelay}, _From, Timers) ->
    {{_Ok, TimerRef}, _Course, _Trainer} = extract_state(Course, Timers),
    Timer = {{_Ok, TimerRef}, Course, Trainer},
    timer:cancel(TimerRef),
    NewTimers = lists:delete(Timer, Timers),
    NewTimerRef = timer:apply_after(NewDelay, ?MODULE, notify_wrapper, [Course, Trainer]),
    % Update the mnesiaDB
    Time = erlang:system_time(millisecond),
    edit_notification(Course, Trainer, NewDelay, Time),
    % Notification to all clients in few seconds to tell that the schedule has changed
    ExpectedTime = NewDelay + Time,
    timer:apply_after(1000, ?MODULE, update_wrapper, [Course, Trainer, ExpectedTime]), % 1 second after the update
    UpdatedTimers = [{NewTimerRef, Course, Trainer} | NewTimers],
    {reply, {edited, ok}, UpdatedTimers};

% Removes timer and removes the tuple representing it from the persistent db
handle_call({delete, Course, Trainer, _}, _From, Timers) ->
    {{_Ok, TimerRef}, _Course, _Trainer} = extract_state(Course, Timers),
    Timer = {{_Ok, TimerRef}, Course, Trainer},
    timer:cancel(TimerRef),
    NewTimers = lists:delete(Timer, Timers),
    io:format("Timers active ~p~n", [NewTimers]),
    % Update the mnesiaDB
    delete_notification(Course),
    {reply, {delited, ok}, NewTimers};

% Returns the current Timers active in the system
handle_call(get_state, _From, Timers) ->
    {reply, Timers, Timers};

% Sets the Timers of the system
handle_call({set_state, NewTimers}, _From, _Timers) ->
    io:format("Timers set to: ~p~n", [NewTimers]),
    {reply, NewTimers, NewTimers};

handle_call(Request, _From, Timers) ->
    {ok, {error, "Unhandled Request", Request}, Timers}.

% Used to notify the users that the course is going to start sending a message to the fitMessanger
notify_wrapper(Course, Trainer) ->
    Args = {Course, Trainer},
      notify(Args).
notify(Args) ->
    Timers = get_state(),
    io:format("Timers situation ~p~n", [Timers]),
    % Remove the Timer from the list
    {Course, Trainer} = Args,
    % Update the mnesiaDB
    delete_notification(Course),
    {{_Ok, TimerRef}, _Course, _Trainer} = extract_state(Course, Timers),
    Timer = {{_Ok, TimerRef}, Course, Trainer},
    NewTimers = lists:delete(Timer, Timers),
    set_state(NewTimers),
    case whereis(fitMessanger) of
        undefined ->
            {reply, {error, "Messanger not found"}, NewTimers};
        _ ->
          Message = {notification, Course, Trainer},
          % Use gen_server:call to send the message to fitMessanger
          Reply = gen_server:call(fitMessanger, Message),
          io:format("Notification for course ~p -> Outcome: ~p~n", [Course, Reply]),
          {noreply, NewTimers}
    end.

% TODO: See if it is necessary or not
% Used to notify the users that the time of the course has changed
update_wrapper(Course, Trainer, ExpectedTime) ->
        Args = {Course, Trainer, ExpectedTime},
          update(Args).
update(Args) -> 
    Timers = get_state(),
    {Course, Trainer, ExpectedTime} = Args,
    case whereis(fitMessanger) of
        undefined ->
            {reply, {error, "Messanger not found"}, Timers};
        _ ->
          Message = {notification, Course, Trainer, ExpectedTime},
          % Use gen_server:call to send the message to fitMessanger
          Reply = gen_server:call(fitMessanger, Message),
          io:format("Notification for course ~p -> Outcome: ~p~n", [Course, Reply]),
          {noreply, Timers}
    end.

handle_cast(_Request, Timers) ->
    {noreply, Timers}.

handle_info(_Info, Timers) ->
    {noreply, Timers}.

terminate(_Reason, _Timers) ->
    ok.

code_change(_OldVsn, Timers, _Extra) ->
    {ok, Timers}.

% Useful functions

get_state() ->
    gen_server:call(?MODULE, get_state).

set_state(Timers) -> 
    gen_server:call(?MODULE, {set_state, Timers}).

extract_state(Course, States) ->
    State = lists:keyfind(Course, 2, States),
    State.