%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  TableName = channels,
  ets:new(TableName, [named_table, public]),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/streamer", cowboy_static, {priv_file, server, "streamer.html"}},
      {"/spectator", cowboy_static, {priv_file, server, "spectator.html"}},
      {"/chat", cowboy_static, {priv_file, server, "chat.html"}},
      {"/ws", sdp_ws, [TableName]},
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