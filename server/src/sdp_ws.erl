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

-record(state, {
  gs,
  partner
}).

%% every connection
init(Req, ClientGS) ->
  {cowboy_websocket, Req, #state{gs = ClientGS, partner = nil}}.


%% every message
websocket_init(#state{gs = GS} = State) ->
%%  Tell server that we are free
  clients_gs:free(GS, self()),
  {ok, State}.


websocket_handle({text, Msg}, #state{gs = GS, partner = Partner} = State) ->
  case Msg of
    <<"{\"message\":\"sleep\"}">> ->
      {reply, {text, <<"Sleeping">>}, State, hibernate};

    <<"{\"message\":\"find_partner\"}">> ->
      case findPartner(GS,Partner) of
        {ok,Reply,NewPartner}->{reply, {text, Reply}, State#state{partner = NewPartner}};
        {error,Reply}->{reply, {text, Reply}, State#state{partner = nil}}
      end;

    <<"{\"candidate\"", _Rest/binary>> ->
      Reply = send_and_reply(Msg,Partner,<<"Candidate sent">>),
      {reply, {text, Reply}, State};

    <<"{\"type\":\"offer\"", _Rest/binary>> ->
      Reply = send_and_reply(Msg,Partner,<<"Offer sent">>),

      {reply, {text, Reply}, State};
    <<"{\"type\":\"answer\"", _Rest/binary>> ->
      Reply = send_and_reply(Msg,Partner,<<"Answer sent">>),
      {reply, {text, Reply}, State};
    _ ->
      io:fwrite("No match~p~n", [Msg]),
      {reply, {text, <<"You said ", Msg/binary>>}, State}
  end;
websocket_handle(_Data, State) ->
  {ok, State}.


websocket_info({message, Pid, need_offer}, #state{partner = nil, gs = GS} = State) ->
  clients_gs:busy(GS, self()),
  Pid ! {message, self(), free},
  {reply, {text, <<"{\"message\":\"need_offer\"}">>}, State#state{partner = Pid}};
websocket_info({message, Pid, need_offer}, #state{partner = _Partner, gs = GS} = State) ->
  clients_gs:busy(GS, self()),
  Pid ! {message, self(), busy},
  {[], State};
websocket_info({message, _Pid, disconnect}, #state{partner = Partner, gs = GS} = State) when is_pid(Partner) ->
  clients_gs:free(GS, self()),
  {reply,{text, <<"{\"message\":\"disconnect\"}">>}, State#state{partner = nil}};

websocket_info({bin_message, _Pid, Bin_Msg}, State) ->
  {reply, [{text, <<Bin_Msg/binary>>}], State};
websocket_info(Info, State) ->
  io:fwrite("Websocket ~p got message ~p~n", [self(), Info]),
  {[], State}.


findPartner(GS,nil) ->
  clients_gs:free(GS,self()),
  case clients_gs:partner(GS, self()) of
    {ok, no_partners} -> {error,<<"{\"reply\":\"no_parnters\"}">>};
    {ok, Pid} ->
      Pid ! {message, self(), need_offer},
      receive
        {message, Pid, busy} ->
          {error,<<"{\"reply\":\"error\"}">>};
        {message, Pid, free} ->
          clients_gs:busy(GS,self()),
          {ok,<<"{\"reply\":\"found\"}">>,Pid}
      end;
    _ -> {error,<<"{\"reply\":\"error\"}">>}
  end;
findPartner(GS,Partner) ->
  Partner ! {message, self(), disconnect},
  findPartner(GS, nil).


send_and_reply(Message, Partner, Reply) ->
  case send2Partner(Message, Partner) of
    ok -> Reply;
    _ -> <<"{\"reply\":\"error\"}">>
  end.


send2Partner(Message, Partner) when is_list(Message) ->
  send2Partner(erlang:list_to_binary(Message), Partner);
send2Partner(Message, Partner) when is_binary(Message) ->
  Alive = is_process_alive(Partner),
  if
    Alive -> Partner ! {bin_message, self(), Message}, ok;
    true -> disconnect
  end;
%%  NewPartners = lists:filter(fun(Pid) ->
%%    Pid /= self() and is_process_alive(Pid) end,
%%    Partners),
%%  lists:foreach(fun(Pid) -> Pid ! {message, self(), Message} end, NewPartners);

send2Partner(_, _) ->
  badarg.
