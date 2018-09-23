%%%-------------------------------------------------------------------
%% @doc brook IP
%% @end
%%%-------------------------------------------------------------------
-module(ip).

-include("interface.hrl").
% source route type
% direct connect route
-define(SOURCE_DIRECT, c).
% static route
-define(SOURCE_STATIC, s).

-define(NEXTHOP_DIRECT, directory_connected).

-define(DEFAULT_ROUTE, 255).

-export([init/0]).
-export([packet/1]).
-export([route/0]).
-export([route/1]).
-export([route/3]).

-record(routing_table, {
  source_route,       % routing learn source
  dest_route,         % destination route
  dest_route_int,     % destination route integer
  subnetmask,         % subnet mask ( number )
  subnetmask_int,     % subnet mask ( number )
  ad,                 % administrative distancec
  metric,             % metric
  nexthop,            % next hop ip
  age,                % destination route learng time
  out_interface       % out put interface
}).

%%====================================================================
%% API
%%====================================================================

init() ->
  {atomic, ok} = mnesia:create_table(routing_table, [{attributes, record_info(fields, routing_table)}, {type, bag}]),
  IfList = interface:list(),
  set_direct_routing_table(IfList, []).

%%--------------------------------------------------------------------

packet(<<_:128, DestIp:32, _/bitstring>> = Data) ->
  receiver_ip(is_self_ip(<<DestIp:32>>), Data).


route() ->
  true.
route(show) ->
  Routes = mnesia:dirty_match_object(routing_table,
    {'_', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11'}),
  Routes.

route(add, static, #{dest_route := {D1, D2, D3, D4}, subnetmask := {S1, S2, S3, S4},
      nexthop := Nexthop, out_interface := OutInterface}=Opt) ->
  <<DestRoute:32>> = <<D1, D2, D3, D4>>,
  <<Subnetmask:32>> = <<S1, S2, S3, S4>>,
  RoutingTable = #routing_table{
    source_route=?SOURCE_STATIC,
    dest_route={D1, D2, D3, D4},
    dest_route_int=DestRoute,
    subnetmask={S1, S2, S3, S4},
    subnetmask_int=Subnetmask,
    ad=1,
    metric=0,
    nexthop=Nexthop,
    age=0,
    out_interface=OutInterface
  },
  set_to_routing_table(RoutingTable),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
% self ip
receiver_ip(true, _) ->
  false;
% other ip
receiver_ip(false, <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16,
      Flg:3, Offset:13, TTL, Protocol, _:16, SourceIp:32, DestIp:32,
      Other/bitstring>>) ->
  {IfName, RouteIp} = get_dest_ip(DestIp),
  case arp:get_mac_addr({IfName, RouteIp}) of
    false ->
      false;
    DestMac ->
      CountTTL = TTL - 1,
      SendData = <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16,
          Flg:3, Offset:13, CountTTL, Protocol, SourceIp:32, DestIp:32>>,
      SendCheckSum = checksum(SendData, 16#0000),
      send_packet(<<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16, Flg:3,
                          Offset:13, CountTTL, Protocol, SendCheckSum:16, SourceIp:32, DestIp:32,
                          Other/bitstring>>, IfName, DestMac)
  end.

%%--------------------------------------------------------------------

is_self_ip(<<D1, D2, D3, D4>>) ->
  case interface:match({'_', '$1', {D1, D2, D3, D4}, '_', '_'}) of
    [] ->
      false;
    _ ->
      true
  end.

%%--------------------------------------------------------------------

get_dest_ip(DestIp) ->
  case get_dest_route(DestIp) of
    {If, ?NEXTHOP_DIRECT, _, _, _} ->
      <<N1, N2, N3, N4>> = <<DestIp:32>>,
      {If, {N1, N2, N3, N4}};
    {If, Nexthop, _, _, _} ->
      {If, Nexthop}
  end.

%%--------------------------------------------------------------------
% icmp protocol
send_packet(<<_:72, 1, _/bitstring>>=Data, IfName, DestMac) ->
    gen_server:cast(packet_sender, {icmp_request, {IfName, DestMac, Data}});
send_packet(<<_:72, 6, _/bitstring>>=Data, IfName, DestMac) ->
    gen_server:cast(packet_sender, {tcp_request, {IfName, DestMac, Data}});
send_packet(<<_:72, 17, _/bitstring>>=Data, IfName, DestMac) ->
    gen_server:cast(packet_sender, {udp_request, {IfName, DestMac, Data}}).

%%--------------------------------------------------------------------
% IP check sump
checksum(<<>>, Sum) ->
  Sum bxor 16#FFFF;
checksum(<<A:16, Other/bitstring>>, Sum) ->
  Check = A + Sum,
  Res = (Check band 16#FFFF) + (Check bsr 16),
  checksum(Other, Res).

%%--------------------------------------------------------------------
get_dest_route(DestIp) ->
  Routes = mnesia:dirty_match_object(routing_table,
          {'_', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11'}),
  Match = match_dest_ip(Routes, DestIp, []),
  fetch_dest_route(Match, {}).

%%--------------------------------------------------------------------
fetch_dest_route([], NowRoute) ->
  NowRoute;
%% fetch to destination route
% directory connected
fetch_dest_route([{_, ?NEXTHOP_DIRECT, _, _, _}=Route| _], _) ->
  Route;
% ad 0
fetch_dest_route([{_, _, _, 0, _}=Route| _], _) ->
  Route;
% not set first routing
fetch_dest_route([Route| Tail], {}) ->
  fetch_dest_route(Tail, Route);

fetch_dest_route([{_, _, _, Ad, Metric}| Tail],
                    {_, _, _, Ad, NowMetric}=Now) when Metric > NowMetric ->
  fetch_dest_route(Tail, Now);

fetch_dest_route([{_, _, _, Ad, _}=Route| Tail],
                    {_, _, _, Ad, _}) ->
  fetch_dest_route(Tail, Route);

fetch_dest_route([{_, _, Subnet, Ad, _}| Tail],
                    {_, _, Subnet, NowAd, _}=Now) when Ad > NowAd ->
  fetch_dest_route(Tail, Now);

fetch_dest_route([{_, _, Subnet, Ad, Metric}| Tail],
                    {_, _, Subnet, NowAd, NowMetric}=Now) when Ad =:= NowAd, Metric > NowMetric ->
  fetch_dest_route(Tail, Now);

fetch_dest_route([{_, _, Subnet, _, _}=Route| Tail],
                    {_, _, Subnet, _, _}) ->
  fetch_dest_route(Tail, Route).

%%--------------------------------------------------------------------
% match to destination ip
% match destination ip for routing table
match_dest_ip([], _, List) ->
  List;
match_dest_ip([{_, _, _, Ip, _, Subnetmask, Ad, Metric, Nexthop, _, If}| Tail],
                  DestIp, List) when is_integer(DestIp) ->
  case DestIp band Subnetmask of
    Ip ->
      match_dest_ip(Tail, DestIp, [{If, Nexthop, Subnetmask, Ad, Metric}|List]);
    _ ->
      match_dest_ip(Tail, DestIp, List)
  end.

%%--------------------------------------------------------------------
%
% set direct routing table
%
set_direct_routing_table([], List) ->
  List;
set_direct_routing_table([{_, _, ?SELEF_IP, _, _}| Tail], List) ->
  set_direct_routing_table(Tail, List);
set_direct_routing_table([{_, _, IP, Netmask, _}| Tail], List)
                        when IP =:= undefined; Netmask =:= undefind ->
  set_direct_routing_table(Tail, List);
set_direct_routing_table([{_, Name, {I1, I2, I3, I4}, {S1, S2, S3, S4}, _}| Tail], List) ->
  <<DestIp:32>> =  <<I1, I2, I3, I4>>,
  <<Subnetmask:32>> = <<S1, S2, S3, S4>>,
  Mask = DestIp band Subnetmask,
  <<M1, M2, M3, M4>> = <<Mask:32>>,
  set_to_routing_table(#routing_table{
    source_route=?SOURCE_DIRECT,
    dest_route={M1, M2, M3, M4},
    dest_route_int=Mask,
    subnetmask={S1, S2, S3, S4},
    subnetmask_int=Subnetmask,
    ad=0,
    metric=0,
    nexthop=?NEXTHOP_DIRECT,
    age=0,
    out_interface=Name
  }),
  set_direct_routing_table(Tail, List).

%%--------------------------------------------------------------------
%
% set to routing table
%
set_to_routing_table(#routing_table{dest_route=Ip, subnetmask=Netmask})
                    when Ip =:= undefined; Netmask =:= undefined ->
  false;
set_to_routing_table(#routing_table{dest_route=Ip, subnetmask=Netmask} = RoutingTable)
                    when is_tuple(Ip), is_tuple(Netmask) ->
  mnesia:transaction(fun() ->
    mnesia:write(routing_table, RoutingTable, write)
  end).

