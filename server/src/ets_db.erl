%%%-------------------------------------------------------------------
%%% @author Ivan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. дек. 2019 20:58
%%%-------------------------------------------------------------------
-module(ets_db).
-author("Ivan").

-record(room, {
  id,
  members
}).

-record(counter, {
  id = 0,
  last_id = 0
}).

%%-record(spectator, {
%%  id,
%%  pid,
%%  channel = nil
%%}).


-export([
  new/0,
  clear/0,
  new_room/0,
  delete_room/1,
  rooms/0
]).

new() ->
  ets:new(rooms, [named_table, {keypos, 2}, public, ordered_set]),
%%  ets:new(spectators, [named_table,{keypos,2},public]),
%%  ets:insert(spectators, #counter{}),
  ets:insert(rooms, #counter{}),
  [new_room() || _ <- lists:seq(1, 5)],
  ok.

clear() ->
  ets:delete(rooms),
  new().


new_room() ->
  NewId = channel_id(),
  Channel = #room{id = NewId, members = []},
  ets:insert(rooms, Channel).

delete_room(ChannelId) ->
  ets:delete(rooms, ChannelId).

rooms() ->
  [{counter, 0, _} | Rooms] = ets:tab2list(rooms),
  Rooms.


channel_id() ->
  ets:update_counter(rooms, 0, {#counter.last_id, 1}).

%% SPECTATOR TABLE
%%spectator_id() ->
%%  ets:update_counter(spectators,0,{#counter.last_id,1}).
%%
%%new_spectator(SpectatorPid) ->
%%  NewId= spectator_id(),
%%  Spectator = #spectator{id =NewId, pid = SpectatorPid},
%%  ets:insert(spectators,Spectator).
%%
%%
%%%% subscribe spectator to channel
%%subscribe(SpectatorId, ChannelId) ->
%%  ets:update_element(spectators,SpectatorId,{#spectator.channel,ChannelId}),
%%  #channel{streamer = Streamer}=ets:lookup(rooms,ChannelId),
%%  #spectator{pid = Pid}=ets:lookup(spectators,SpectatorId),
%%  Pid ! {message, self(),{join,Streamer}}.
%%
%%%% subscribe spectator to channel
%%unsubscribe(SpectatorId) ->
%%  ets:update_element(spectators,SpectatorId,{#spectator.channel,nil}),
%%  #spectator{pid = Pid}=ets:lookup(spectators,SpectatorId),
%%  Pid ! {message, self(),disconnect}.