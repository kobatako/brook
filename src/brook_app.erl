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
  io:format("~p~n", [Listen]),
  save_interface(Listen, Interface),
  arp:init(),
  ip:init(),
  brook_sup:start_link(make_bind(Listen, [])).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

% make bind file descriptor
make_bind([],  Res) ->
  Res;
make_bind([{interface, _, undefined, _, _}| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([{interface, _, _, undefined, _}| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([{interface, _, {127, 0, 0, 1}, _, _}| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([{_, IfName, Addr, _, MacAddr}|Tail], Res) ->
  {ok, IPFD} = procket:open(0, [
    {protocol, ?ETH_P_ALL},
    {type, raw},
    {family, packet}
  ]),
  ok = packet:bind(IPFD, packet:ifindex(IPFD, IfName)),
  make_bind(Tail, [#{if_name => IfName, ip_fd => IPFD, mac_addr => MacAddr}| Res]).

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
  io:format("~p~n", [Elm]),
  interface_opt(Opts, #interface{name=Name}).

%
% interface option
%
interface_opt([], Opt) ->
  Opt;
interface_opt([Head| Tail], Opt) ->
  Res = case Head of
    {hwaddr, Hwaddr} ->
      Opt#interface{hw_addr=Hwaddr};
    % IP v4
    {addr, {_, _, _, _}=Addr} ->
      Opt#interface{addr=Addr};
    % IP v4
    {netmask, {_, _, _, _}=Netmask} ->
      Opt#interface{netmask=Netmask};
    _ ->
      Opt
  end,
  interface_opt(Tail, Res).

