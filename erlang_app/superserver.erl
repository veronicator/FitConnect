-module(superserver).
-behaviour(supervisor).

-export([start_link/0, start_link_shell/0, init/1]).

start_link() ->
    %application:start(mnesia),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link_shell()->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    unlink(Pid).

% Initialization
init([]) ->
    % Define supervisor behaviour
    SupFlags = #{strategy => one_for_one,
        intensity => 3,
        period => 20},

    % Define child behaviour for the clients that exchange messages
    FitMessanger = #{id => fitMessanger,
        start => {fitMessanger, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,   
        modules => [fitMessanger]},
    
    % Define child behaviour for the Timers
    FitNotifier = #{id => fitNotifier,
        start => {fitNotifier, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,   
        modules => [fitNotifier]},

    Children = [FitMessanger, FitNotifier],

{ok, {SupFlags, Children}}.