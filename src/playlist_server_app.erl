-module(playlist_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
      {'_', [{"/", hello_handler, []}, {"/playlist/:name", [{name, nonempty}], playlist_handler, []}]}
    ]),
  {ok, _} = cowboy:start_clear(my_http_listener, 100,
      [{port, 8080}], #{env => #{dispatch => Dispatch}}
    ),
	playlist_server_sup:start_link().

stop(_State) ->
	ok.
