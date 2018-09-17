%%%-------------------------------------------------------------------
%% @doc brook public API
%% @end
%%%-------------------------------------------------------------------

-module(brook_app).

-include("interface.hrl").
-behaviour(application).

-define(ETH_P_ALL, 16#0300).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, IF} = inet:getifaddrs(),
  Listen = lists:map(fun(Elm) -> interfaceList(Elm) end, IF),
  Interface = ets:new(interface, [set, public, {keypos, #interface.name}, named_table]),
  saveInterface(Listen, Interface),
  {ok, FD} = procket:open(0, [
    {protocol, ?ETH_P_ALL},
    {type, raw},
    {family, packet}
  ]),
  arp:init(),
  ip:init(),
  brook_sup:start_link(FD).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

saveInterface([], _) ->
  true;
saveInterface([Head| Tail], Interface) ->
  ets:insert_new(Interface, Head),
  saveInterface(Tail, Interface).

%%
%% debug print interface
%%
printInterface(IF, Interface) ->
  case ets:next(Interface, IF) of
    '$end_of_table' ->
      true;
    Next ->
      printInterface(Next, Interface)
  end.

%
% interface list
%
interfaceList(Elm) ->
  {Name, Opts} = Elm,
  interfaceOpt(Opts, #interface{name=Name}).

%
%
%
interfaceOpt([], Opt) ->
  Opt;
interfaceOpt([Head| Tail], Opt) ->
  Res = case Head of
    {hwaddr, Hwaddr} ->
      Opt#interface{hw_addr=Hwaddr};
    {addr, Addr} ->
      Opt#interface{addr=Addr};
    {netmask, Netmask} ->
      Opt#interface{netmask=Netmask};
    _ ->
      Opt
  end,
  interfaceOpt(Tail, Res).

