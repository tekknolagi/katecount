%%%-------------------------------------------------------------------
%% @doc katecount top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(katecount_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Id, Mod, Args, Restart, Type),
        {Id, {Mod, start_link, Args}, Restart, 60000, Type, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    io:format("init~n"),
    MaxRestart = 0,
    MaxTime = 1,
    % RestartStrategy = {one_for_all, MaxRestart, MaxTime},
    RestartStrategy = {one_for_one, 1, 5},
     Daemon =
       {node, {daemon, start_link, []}, permanent, 2000, worker, [daemon]},
    StateHandler =
      ?CHILD(state_handler, state_handler, [], transient, worker),
    Bill =
      ?CHILD(katecount_bill, katecount_bill, [], transient, worker),
    Children = [StateHandler, Bill],
    % daemon:boot(),
    {ok, {RestartStrategy, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================
