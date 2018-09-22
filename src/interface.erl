-module(interface).

-include("interface.hrl").

-export([init/0]).

%%====================================================================
%% API
%%====================================================================

init() ->
  {ok, IF} = inet:getifaddrs(),
  Listen = [interface_list(Elm) || Elm <- IF],
  Interface = ets:new(interface, [set, public, {keypos, #interface.name}, named_table]),
  save_interface(Listen, Interface),
  make_bind(Listen, []).

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
make_bind([{interface, _, ?SELEF_IP, _, _}| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([{_, IfName, _, _, MacAddr}|Tail], Res) ->
  {ok, IPFD} = procket:open(0, [
    {protocol, ?ETH_P_ALL},
    {type, raw},
    {family, packet}
  ]),
  ok = packet:bind(IPFD, packet:ifindex(IPFD, IfName)),
  make_bind(Tail, [#{if_name => IfName, ip_fd => IPFD, mac_addr => MacAddr}| Res]).

%%--------------------------------------------------------------------

save_interface([], _) ->
  true;
save_interface([Head| Tail], Interface) ->
  ets:insert_new(Interface, Head),
  save_interface(Tail, Interface).

%%--------------------------------------------------------------------
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

