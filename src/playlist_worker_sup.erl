-module(playlist_worker_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name) when is_atom(Name) ->
    supervisor:start_child(?SERVER, [Name]).

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy,
              MaxRestarts,
              MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

	Procs = [{playlist_worker, {playlist_worker, start_link, []}, Restart, Shutdown, Type, [playlist_worker]}],
	{ok, {SupFlags, Procs}}.
