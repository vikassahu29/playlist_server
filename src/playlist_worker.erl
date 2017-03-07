-module(playlist_worker).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([add_song/2]).
-export([remove_song/2]).
-export([play_song/2]).
-export([get_songs/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
songs=[],
current_song=""
}).

%% API.


start_link(PlaylistKey) when is_atom(PlaylistKey) ->
	gen_server:start_link({local, PlaylistKey}, ?MODULE, [], []).


add_song(Playlist, Song) ->
  Pid = find_process_for_playlist(Playlist),
  gen_server:call(Pid, {add_song, Song}).

remove_song(Playlist, Song) ->
  Pid = find_process_for_playlist(Playlist),
  gen_server:call(Pid, {remove_song, Song}).

play_song(Playlist, Song) ->
  Pid = find_process_for_playlist(Playlist),
  gen_server:call(Pid, {play_song, Song}).

get_songs(Playlist) ->
  Pid = find_process_for_playlist(Playlist),
  gen_server:call(Pid, get_songs).
%% gen_server.

init([]) ->
	{ok, #state{songs=[], current_song=""}}.

handle_call({add_song, Link}, _From, State) ->
	{reply, song_added, add_song_helper(State, Link)};

handle_call({remove_song, Link}, _From, State) ->
	{reply, song_removed, remove_song_helper(State, Link)};

handle_call({change_song, Link}, _From, State) ->
	{reply, song_changed, play_song_helper(State, Link)};

handle_call(get_songs, _From, State=#state{songs=Songs}) ->
	{reply, Songs, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


add_song_helper(#state{songs=Songs, current_song=Current}, Link) ->
  NewSongs = [Link| Songs],
  #state{songs=NewSongs, current_song=Current}.

remove_song_helper(#state{songs=Songs, current_song=Current}, Link) ->
  NewSongs = lists:delete(Link, Songs),
  #state{songs=NewSongs, current_song=Current}.

play_song_helper(#state{songs=Songs}, Link) ->
  #state{songs=Songs, current_song=Link}.

find_process_for_playlist(Playlist) ->
    PlaylistKey = get_registered_name_for_playlist(Playlist),
    case whereis(PlaylistKey) of
        undefined -> register_playlist(PlaylistKey);
        Pid when is_pid(Pid) -> Pid
    end.

get_registered_name_for_playlist(Playlist) ->
    list_to_atom("playlist_name::" ++ Playlist).

register_playlist(Playlist) ->
    {ok, Pid} = playlist_worker_sup:start_child(Playlist),
    Pid.
