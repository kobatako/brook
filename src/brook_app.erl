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
  Listen = lists:map(fun(Elm) -> interface_list(Elm) end, IF),
  Interface = ets:new(interface, [set, public, {keypos, #interface.name}, named_table]),
  save_interface(Listen, Interface),
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

save_interface([], _) ->
  true;
save_interface([Head| Tail], Interface) ->
  ets:insert_new(Interface, Head),
  save_interface(Tail, Interface).

%%
%% debug print interface
%%
print_interface(IF, Interface) ->
  case ets:next(Interface, IF) of
    '$end_of_table' ->
      true;
    Next ->
      print_interface(Next, Interface)
  end.

%
% interface list
%
interface_list(Elm) ->
  {Name, Opts} = Elm,
  interface_opt(Opts, #interface{name=Name}).

%
%
%
interface_opt([], Opt) ->
  Opt;
interface_opt([Head| Tail], Opt) ->
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
  interface_opt(Tail, Res).

