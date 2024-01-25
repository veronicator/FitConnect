-module(fitMessanger).
-behaviour(gen_server).

-compile([debug_info]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Called by the supervisor to start the fitMessanger
start_link() ->
    gen_server:start_link({local, fitMessanger}, ?MODULE, [], []).

% Does nothing because it satrts empty
init([]) ->
    {ok, []}.

% Connects the user to all his/her courses
handle_call({From, connectToAllCourses, Courses, Username}, _From, Clients) ->
    NewClients = lists:foldl(
        fun(Course, Acc) ->
            users,
            getUsers(filterByCourse(Acc, Course)),
            % Adds to "Clients" -> {Course, Username, From}
            [{Course, Username, From} | Acc]
        end,
        Clients,
        Courses
    ),
    {reply, {connection, ok}, NewClients};

% Generates if not present the course and then connects the user
handle_call({From, connectToCourse, Course, Username}, _From, Clients) ->
    users,
    getUsers(filterByCourse(Clients, Course)),
    broadcast(join, filterByCourse(Clients, Course), {Username}),
    % Adds to "Clients" -> {Course, Username, From}
    {reply, {connection, ok}, [{Course, Username, From} | Clients]};

% Displays all users connected to the platform (DEBUG)
handle_call({users}, _From, Clients) ->
    Reply = {users, getUsers(Clients)},
    {reply, Reply, Clients};

% Displays all opened courses (DEBUG)
handle_call({courses}, _From, Clients) ->
    Reply = {courses, getCourses(Clients)},
    {reply, Reply, Clients};

% Displays all Clients structures (DEBUG)
handle_call({clients}, _From, Clients) ->
    Reply = {clients, Clients},
    {reply, Reply, Clients};

% Receives a message and broadcasts it to all users of the same course
handle_call({send, Msg, User, course, Course}, _From, Clients) ->
    broadcast(new_msg, remove(User, filterByCourse(Clients, Course)), {User, Msg}),
    {reply, {send, ok}, Clients};

% TODO: ???????
%sending the msg to direct -> FIX SENDS N TIMES DEPENDING ON NUMBER OF GROUPS FORSE
%handle_call({send, Msg, Sender, user, User}, _From, Clients) ->
    %Users = filterByUser(Clients, User),
    %broadcast(new_msg, Users, {Sender, Msg}),
    %{reply, {send, ok}, Clients};

% Removes the state of the user+course, so the user won't receive messages from the course
handle_call({exit, Course, User}, _From, Clients) ->
    NewClients = remove(User, Course, Clients),
    broadcast(disconnect, filterByCourse(NewClients, Course), {User}),
    {reply, {exit, ok}, NewClients};

% Disconnect the user from the Server. removing all his states from the general state
handle_call({disconnect, User}, _From, Clients) ->
    Courses = getCourses(User),
    NewClients = lists:foldl(
        fun(Course, Acc) ->
            % Removes from "Clients" -> {Course, Username, From}
            remove(User, Course, Acc)
        end,
        Clients,
        Courses
    ),
    {reply, {disconnection, performed}, NewClients};

% Forwards a notification to a chat group as a reminder
handle_call({notification, Course, Trainer}, _From, Clients) ->
    io:format("Notification for course: ~p with trainer: ~p~n", [Course, Trainer]),
    AllUsers = filterByCourse(Clients, Course),
    %io:format("Clients found: ~p, we need to remove the one with the trainer~n", [AllUsers]),
    Users = removeTrainer(Trainer, AllUsers),
    io:format("Final Clients: ~p~n", [Users]),
    broadcast({notification, {course, Course}}, Users),
  {reply, {ok}, Clients};

% Forwards a notification to a chat group to notify that the time of the course changed
handle_call({notification, Course, Trainer, NewTime}, _From, Clients) ->
    io:format("Trainer ~p updated the time to: ~p for the course: ~p~n", [Trainer, NewTime, Course]),
    AllUsers = filterByCourse(Clients, Course),
    %io:format("Clients found: ~p, we need to remove the one with the trainer~n", [AllUsers]),
    Users = removeTrainer(Trainer, AllUsers),
    io:format("Final Clients: ~p~n", [Users]),
    broadcast({notification, {course, Course, NewTime}}, Users),
  {reply, {ok}, Clients};
  
handle_call(Request, _From, State) ->
    {ok, {error, "Unhandled Request", Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("Received message: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Useful functions

remove(User, Course, [H | T]) ->
    {Coursename, Username, _} = H,
    if
        (Username == User) and (Coursename == Course) ->
            T;
        true ->
            [H | remove(User, Course, T)]
    end.

remove(User, [H | T]) ->
    {_, Username, _} = H,
    if
        Username == User ->
            T;
        true ->
            remove(User, T ++ [H])
    end.

%filterByUser(Clients, User) ->
    %filterByUser(Clients, User, []).

%filterByUser([], _, Result) ->
    %Result;
%filterByUser([H | T], User, Result) ->
    %{_, U, _} = H,
    %if
        %U == User ->
            %filterByUser(T, User, [H | Result]);
        %true ->
            %filterByUser(T, User, Result)
    %end.

getCourses(Clients) ->
    getCourses(Clients, []).

getCourses([], Result) ->
    Result;
getCourses([H | T], Result) ->
    {Course, _, _} = H,
    Found = find(Result, Course),
    if
        Found == true ->
            getCourses(T, Result);
        true ->
            getCourses(T, [Course | Result])
    end.

getUsers(Clients) ->
    getUsers(Clients, []).

getUsers([], Result) ->
    Result;
getUsers([H | T], Result) ->
    {_, User, _} = H,
    getUsers(T, [User | Result]).

find([], _) ->
    false;
find([H | T], Identifier) ->
    Existing = H,
    if
        Identifier == Existing ->
            true;
        true ->
            find(T, Identifier)
    end.

filterByCourse(L, Course) ->
    filterByCourse(L, Course, []).

filterByCourse([], _, Result) ->
    Result;
filterByCourse([H | T], Course, Result) ->
    {R, _, _} = H,
    if
        R == Course ->
            filterByCourse(T, Course, [H | Result]);
        true ->
            filterByCourse(T, Course, Result)
    end.

removeTrainer(Trainer, Clients) ->
    lists:filter(fun({_, TrainerVal, _}) -> TrainerVal /= Trainer end, Clients).

broadcast(join, Clients, {User}) ->
    broadcast({info, {user_joined, User}}, Clients);
broadcast(new_msg, Clients, {User, Msg}) ->
    broadcast({new_msg, {msg_received, User, Msg}}, Clients);
broadcast(disconnect, Clients, {User}) ->
    broadcast({info, {user_left, User}}, Clients).

broadcast(Msg, Clients) ->
    lists:foreach(fun({_, _, Pid}) -> Pid ! Msg end, Clients).
