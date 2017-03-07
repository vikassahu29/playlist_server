-module(playlist_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_all,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Children = [playlist_worker_supervisor()],
	{ok, {SupFlags, Children}}.

playlist_worker_supervisor() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    {playlist_worker_sup, {playlist_worker_sup, start_link, []},
        Restart, Shutdown, Type, [playlist_worker_sup]}.
