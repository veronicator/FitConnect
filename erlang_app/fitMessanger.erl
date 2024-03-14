-module(fitMessanger).
-behaviour(gen_server).

-compile([debug_info]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Called by the supervisor to start the fitMessanger
start_link() ->
    gen_server:start_link({local, fitMessanger}, ?MODULE, [], []).

% Does nothing because it starts empty
init([]) ->
    {ok, []}.

handle_call({clean}, _From, _Clients) ->
    Clients = [],
    {noreply, Clients};

% Connects the user to all his/her courses
% [{["2","3","5"],"username",#Pid<Ref>}]
handle_call({From, connect, Courses, Username}, _From, Clients) ->
    {reply, {connection, ok}, [{Courses, Username, From} | Clients]};

% Displays all Clients structures in the state od the server (DEBUG)
handle_call({clients}, _From, Clients) ->
    Reply = {clients, Clients},
    {reply, Reply, Clients};

% Generates if not present the course and then connects the user
handle_call({joinCourse, Course, Username}, _From, Clients) ->
    NewClients = iterate_over_connected_clients(joinCourse, {Course, Username}, Clients),
    %io:format("Result: ~p~n", [NewClients]), % DEBUG
    {reply, {connection, ok}, NewClients};

% Receives a message and broadcasts it to all users of the same course
handle_call({sendToCourse, Course, Username, Msg}, _From, Clients) ->
    iterate_over_connected_clients(sendToCourse, {Course, Username, Msg}, Clients),
    {reply, {send, ok}, Clients};

% Removes from client's structure the course that he/she is exiting
handle_call({exitCourse, Course, Username}, _From, Clients) ->
    NewClients = iterate_over_connected_clients(exitCourse, {Course, Username}, Clients),
    %io:format("Result: ~p~n", [NewClients]), % DEBUG
    {reply, {exit, ok}, NewClients};

% Forwards a notification to a chat group as a reminder
handle_call({schedule, Mode, Username, ScheduleId, Timestamp}, _From, Clients) ->
    Msg = {Mode, ScheduleId, Timestamp}, % Mode is atomic, so can be used as identifier
    Result = find_client(Clients, Username), % We need to handle if client is not present
    if
        Result == false ->
            io:format("Client not available~n"),
            Response = notok;
        true ->
            {_Courses, _User, Pid} = Result,
            Pid ! Msg, % Send message to Pid
            Response = ok
    end,
    {reply, {Response}, Clients};

% Disconnect the user from the Server. removing all his states from the general state
handle_call({disconnect, Username}, _From, Clients) ->
    User = find_client(Clients, Username),
    %io:format("User: ~p~n", [User]), % DEBUG
    NewClients =
        if 
            User == false ->
                Clients;
            true ->
                lists:delete(User, Clients)
        end,
    Msg =
        if 
            User == false ->
                {disconnection, error};
            true ->
                {disconnection, performed}
        end,
    {reply, Msg, NewClients};


handle_call(Request, _From, Clients) ->
    io:format("Received unhandled request: ~p;~n", [Request]),
    {reply, {unhandled_request, Request}, Clients}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("Received message: ~p;~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Useful functions

% Possible inputs:
% joinCourse, {Course, Username}, Clients -> NewClients
% sendToCourse, {Course, Username, Msg}, Clients
% exitCourse, {Course, Username}, Clients -> newClients
iterate_over_connected_clients(Mode, Data, Clients) ->
    case Mode of
        joinCourse ->
            {Course, Username} = Data,
            Msg = {userJoined, Course, Username};
        sendToCourse ->
            {Course, Username, Text} = Data,
            Msg = {message, Course, Username, Text};
        exitCourse ->
            {Course, Username} = Data,
            Msg = {userExited, Course, Username}
    end,
    {Receivers, OldUser, NewUser} = lists:foldl(fun(Client, {Acc, OldUser, NewUser}) ->
            {Courses, User, Pid} = Client,
            %io:format("Client: ~p~n", [Client]), %DEBUG
            if
                % Update user's structure in the state of the server
                User == Username ->
                    case Mode of
                        joinCourse ->
                            NUser = {[Course | Courses], User, Pid}, % New user with added course
                            {Acc, Client, NUser}; % Old user is the client, New user has the updated courses
                        sendToCourse ->
                            {Acc, OldUser, NewUser}; % Do nothing, just returns list 
                        exitCourse ->
                            NCourses = lists:delete(Course, Courses),
                            NUser = {NCourses, User, Pid}, % New user without the course selected
                            io:format("NUser: ~p~n", [NUser]), %DEBUG
                            {Acc, Client, NUser} % Old user is the client, New user has the updated courses
                    end;
                % Check if is in the course 
                true -> 
                    Found = element_is_present(Courses, Course),
                    if
                        Found == true ->
                            {[Pid | Acc], OldUser, NewUser}; % append users that need to receive the messages
                    true ->
                        {Acc, OldUser, NewUser} % Does nothing, just returns list
                    end
            end
        end, {[], [], []}, Clients),
    % NEED TO HANDLE sendToCourse & exitCourse, because they need to be updated differently !!
    %io:format("Receivers: ~p;~nNew: ~p;~nOld: ~p;~n", [Receivers, NewUser, OldUser]), % DEBUG 
    broadcast(Msg, Receivers),
    if
        Mode == sendToCourse ->
            ok; % We don't need to update the state of the server
        true ->
            ProvClients = lists:delete(OldUser, Clients),
            io:format("Client: ~p;~n", [ProvClients]),
            AddedUser = [NewUser | ProvClients],
            io:format("Client: ~p;~n", [AddedUser]),
            AddedUser % We update the state of the server removing the old structure and adding a new one
        end.

element_is_present(List, Element) ->
    lists:any(fun(X) -> X == Element end, List).

find_client(Clients, Username) ->
    case lists:filtermap(
                fun(Client) ->
                    io:format("Client: ~p;~n", [Client]),
                    {_Courses, User, _Pid} = Client,
                    User == Username
                end, Clients) of
        [] -> false;
        [FoundClient] -> FoundClient
    end.
    
    
broadcast(Msg, Pids) ->
    lists:foreach(fun(Pid) -> Pid ! Msg end, Pids).