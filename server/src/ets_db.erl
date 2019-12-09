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

-record(channel, {
  id,
  name,
  streamer
}).

-record(counter, {
  id = 0,
  last_id = 0
}).

-record(spectator, {
  id,
  pid,
  channel = nil
}).


-export([
  new/0,
  clear/0,
  new_channel/2,
  delete_channel/1,
  channels/0
]).

new() ->
  ets:new(channels, [named_table, {keypos, 2}, public]),
%%  ets:new(spectators, [named_table,{keypos,2},public]),
%%  ets:insert(spectators, #counter{}),
  ets:insert(channels, #counter{}).

clear() ->
  ets:delete_all_objects(channels).

new_channel(Streamer, Name) ->
  NewId = channel_id(),
  Channel = #channel{id = NewId, name = Name, streamer = Streamer},
  ets:insert(channels, Channel).

delete_channel(ChannelId) ->
  ets:delete(channels, ChannelId).


channels() ->
  ets:tab2list(channels).


channel_id() ->
  ets:update_counter(channels, 0, {#counter.last_id, 1}).

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
%%  #channel{streamer = Streamer}=ets:lookup(channels,ChannelId),
%%  #spectator{pid = Pid}=ets:lookup(spectators,SpectatorId),
%%  Pid ! {message, self(),{join,Streamer}}.
%%
%%%% subscribe spectator to channel
%%unsubscribe(SpectatorId) ->
%%  ets:update_element(spectators,SpectatorId,{#spectator.channel,nil}),
%%  #spectator{pid = Pid}=ets:lookup(spectators,SpectatorId),
%%  Pid ! {message, self(),disconnect}.