%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok,Pid}=clients_gs:start_link(),
  Dispatch = cowboy_router:compile([
    {'_', [
%%      {"/streamer", cowboy_static, {priv_file, server, "streamer.html"}},
%%      {"/spectator", cowboy_static, {priv_file, server, "spectator.html"}},
      {"/ws", sdp_ws, Pid},
      {"/", cowboy_static, {priv_file, server, "index.html"}},
      {"/static/[...]", cowboy_static, {priv_dir, server, "static"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),

  server_sup:start_link().

stop(_State) ->
  ok.

%% internal functions