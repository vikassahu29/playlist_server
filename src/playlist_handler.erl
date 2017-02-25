-module(playlist_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  HasBody = cowboy_req:has_body(Req0),
  Req = handle_request(Method, HasBody, Req0),
	{ok, Req, State}.

handle_request(<<"POST">>, true, Req0) ->
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                      <<"POST">>,
                      Req0);

handle_request(<<"GET">>, false, Req0) ->
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                      <<"GET">>,
                      Req0);

handle_request(<<"DELETE">>, false, Req0) ->
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                      <<"DELETE">>,
                      Req0);

handle_request(<<"PUT">>, false, Req0) ->
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                      <<"PUT">>,
                      Req0).
