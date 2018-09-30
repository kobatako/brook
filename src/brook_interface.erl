%%%-------------------------------------------------------------------
%% @doc brook interface
%% @end
%%%-------------------------------------------------------------------
-module(brook_interface).

-include("interface.hrl").

-export([init/0]).
-export([list/0]).
-export([match/1]).
-export([match_network/1]).

%%====================================================================
%% API
%%====================================================================

init() ->
  {ok, IF} = inet:getifaddrs(),
  Lists = [interface_list(Elm) || Elm <- IF],
  mnesia:create_table(interface, [{attributes, record_info(fields, interface)}]),
  save_interface(Lists),
  make_bind(Lists, []).

list() ->
  mnesia:dirty_match_object(interface, {'$1', '$2', '$3', '$4', '$5', '$6'}).

match(Match) ->
  mnesia:dirty_match_object(interface, Match).

match_network(Addr) ->
  fetch_interface_network(list(), Addr).

%%====================================================================
%% Internal functions
%%====================================================================

fetch_interface_network([], _) ->
  not_match;
fetch_interface_network([#interface{netaddr=NetAddr, addr=IfAddr}| Tail], Addr) ->
  case NetAddr band Addr of
   NetAddr ->
      #{addr=>IfAddr};
    _ ->
      fetch_interface_network(Tail, Addr)
  end.

%%--------------------------------------------------------------------
% make bind file descriptor
make_bind([],  Res) ->
  Res;
make_bind([{interface, _, undefined, _, _, _}| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([{interface, _, _, undefined, _, _}| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([{interface, _, ?SELEF_IP, _, _, _}| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([{_, IfName, _, _, MacAddr, _}|Tail], Res) ->
  {ok, IPFD} = procket:open(0, [
    {protocol, ?ETH_P_ALL},
    {type, raw},
    {family, packet}
  ]),
  ok = packet:bind(IPFD, packet:ifindex(IPFD, IfName)),
  make_bind(Tail, [#{if_name => IfName, ip_fd => IPFD, mac_addr => MacAddr}| Res]).

%%--------------------------------------------------------------------
%
%
%
save_interface([]) ->
  true;
save_interface([Head| Tail]) ->
  mnesia:transaction(fun() ->
    mnesia:write(interface, Head, write)
  end),
  save_interface(Tail).

%%--------------------------------------------------------------------
%
% interface list
%
interface_list(Elm) ->
  {Name, Opts} = Elm,
  interface_opt(Opts, #interface{name=Name}).

%%--------------------------------------------------------------------
%
% interface option
% set interface option, macaddress, ip address and netmask
%
interface_opt([], Opt) ->
  interface_opt_netaddr(Opt);
interface_opt([Head| Tail], Opt) ->
  Res = case Head of
    {hwaddr, Hwaddr} ->
      Opt#interface{hw_addr=brook_ethernet:trance_to_tuple_mac_addr(Hwaddr)};
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

%%--------------------------------------------------------------------
%
% interface option
% set interface option, netaddress.
%
interface_opt_netaddr(#interface{
  addr = Addr,
  netmask = Netmask}=Opt
) when Addr =:= undefined; Netmask =:= undefined ->
  Opt#interface{netaddr=undefined};

interface_opt_netaddr(#interface{
  addr = {A1, A2, A3, A4},
  netmask = {N1, N2, N3, N4}
}=Opt) ->
  <<A:32>> = <<A1, A2, A3, A4>>,
  <<N:32>> = <<N1, N2, N3, N4>>,
  Opt#interface{netaddr=A band N}.


