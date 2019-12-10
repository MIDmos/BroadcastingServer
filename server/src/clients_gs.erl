%%%-------------------------------------------------------------------
%%% @author Ivan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. дек. 2019 0:13
%%%-------------------------------------------------------------------
-module(clients_gs).
-author("Ivan").

-behavior(gen_server).
%%----------------------------------------------------------------------------
%% BEHAVIOUR EXPORTS
%%----------------------------------------------------------------------------
-export([
  init/1,
  terminate/2,
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

%%----------------------------------------------------------------------------
%% PUBLIC API EXPORTS
%%----------------------------------------------------------------------------
-export([
  free/2,
  busy/2,
  partner/2,
  start_link/0
]).
%%----------------------------------------------------------------------------
%% STRUCTURES
%%----------------------------------------------------------------------------
-record(state, {
  free
}).


%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
free(P, WebSocketPid) ->
  gen_server:cast(P, {free, WebSocketPid}).

busy(P, WebSocketPid) ->
  gen_server:cast(P, {busy, WebSocketPid}).

partner(P, WebSocketPid) ->
  gen_server:call(P, {partner, WebSocketPid}).


start_link() ->
  gen_server:start_link(?MODULE, [], []).


%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init([]) ->
  {ok, #state{free = sets:new()}}.

terminate(_Reason, _State) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
handle_call({partner, WebSocketPid}, _From, #state{free = Free} = State) ->
  AliveFree = sets:filter(fun(Pid) -> is_process_alive(Pid) end, Free),
  FreeList = sets:to_list(AliveFree),
  Partner = find_partner(WebSocketPid, FreeList),
  case Partner of
    {ok, no_partners} -> {reply, Partner, State#state{free = AliveFree}};
    {ok, FreePid} ->
      NewFree = sets:del_element(FreePid, AliveFree),
      {reply, Partner, State#state{free = NewFree}}
  end;

handle_call(_, _From, State) ->
  {reply, {error, wrong_request}, State}.

handle_cast({busy, WebSocketPid}, #state{free = Free} = State) ->
  NewFree = sets:del_element(WebSocketPid, Free),
  {noreply, State#state{free = NewFree}};

handle_cast({free, WebSocketPid}, #state{free = Free} = State) ->
  NewFree = sets:add_element(WebSocketPid, Free),
  {noreply, State#state{free = NewFree}}.



handle_info(_, State) ->
  {noreply, State}.


find_partner(_Pid, []) ->
  {ok, no_partners};
find_partner(Pid, [Pid | T]) ->
  find_partner(Pid, T);
find_partner(_Pid, [FreePid | _]) ->
  {ok, FreePid}.