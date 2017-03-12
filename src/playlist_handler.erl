-module(playlist_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  HasBody = cowboy_req:has_body(Req0),
  Req = handle_request(Method, HasBody, Req0),
	{ok, Req, State}.

handle_request(<<"POST">>, true, Req0) ->
  Name = binary_to_list(cowboy_req:binding(name, Req0)),
  {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
  Link = proplists:get_value(<<"link">>, PostVals),
  playlist_worker:add_song(Name, Link),
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                      <<"POST">>,
                      Req);

handle_request(<<"GET">>, false, Req0) ->
  Name = binary_to_list(cowboy_req:binding(name, Req0)),
  Result = playlist_worker:get_songs(Name),
  cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                      jiffy:encode(Result),
                      Req0);

handle_request(<<"DELETE">>, true, Req0) ->
  Name = binary_to_list(cowboy_req:binding(name, Req0)),
  {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
  Link = proplists:get_value(<<"link">>, PostVals),
  playlist_worker:remove_song(Name, Link),
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                      <<"DELETE">>,
                      Req);

handle_request(<<"PUT">>, false, Req0) ->
  Name = binary_to_list(cowboy_req:binding(name, Req0)),
  {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
  Link = proplists:get_value(<<"link">>, PostVals),
  playlist_worker:play_song(Name, Link),
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                      <<"PUT">>,
                      Req).
