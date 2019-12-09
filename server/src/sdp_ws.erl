%%%-------------------------------------------------------------------
%%% @author Ivan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. дек. 2019 11:06
%%%-------------------------------------------------------------------
-module(sdp_ws).
-author("Ivan").

%% API
-behavior(cowboy_websocket).

-export([
  init/2,
  websocket_init/1,
  websocket_handle/2,
  websocket_info/2]).


%% every connection
init(Req, State) ->
  io:fwrite("init ~p~n", [self()]),
  {cowboy_websocket, Req, State}.


%% every message
websocket_init([Name] = State) ->
  io:fwrite("ws_init ~p~n", [self()]),
  ets:insert(Name, {self()}),
  {ok, State}.


websocket_handle({text, Msg}, [Name] = State) ->
  case Msg of
    <<"{\"message\":\"new\"}">> ->
      resend2others(Msg, Name),
      {reply, {text, <<"New sent">>}, State};
    <<"{\"candidate\"", _Rest/binary>> ->
      resend2others(Msg, Name),
      {reply, {text, <<"Candidate sent">>}, State};
    <<"{\"type\":\"offer\"", _Rest/binary>> ->
      resend2others(Msg, Name),
      {reply, {text, <<"Offer sent">>}, State};
    <<"{\"type\":\"answer\"", _Rest/binary>> ->
      resend2others(Msg, Name),
      {reply, {text, <<"Answer sent">>}, State};
    _ ->
      io:fwrite("No match~p~n", [Msg]),
      {reply, {text, <<"You said ", Msg/binary>>}, State}
  end;
websocket_handle(_Data, State) ->
  {ok, State}.


websocket_info({message, _Pid, Bin_Msg}, State) ->
  {reply, [{text, <<Bin_Msg/binary>>}], State};
websocket_info(Info, State) ->
  io:fwrite("Websocket ~p got message ~p~n", [self(), Info]),
  {[], State}.


resend2others(Message, TbName) when is_list(Message) ->
  resend2others(erlang:list_to_binary(Message), TbName);
resend2others(Message, TbName) when is_binary(Message) ->
  Others = lists:filter(fun({Pid}) -> Pid /= self() end, ets:tab2list(TbName)),
  lists:foreach(fun({Pid}) ->
    Alive = is_process_alive(Pid),
    if
      Alive ->
        Pid ! {message, self(), Message};
      true ->
        ets:delete(TbName, Pid)
    end
                end, Others);

resend2others(_, _) ->
  badarg.
