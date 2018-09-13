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

-define(SELEF_IP, {127, 0, 0, 1}).

-export([init/0, get_dest_ip/1]).

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

init() ->
  ets:new(routing_table, [duplicate_bag, protected, {keypos, #routing_table.out_interface}, named_table]),
  IfList = ets:match(interface, {'$1', '$2', '$3', '$4', '$5'}),
  set_direct_routing_table(IfList, []).

get_dest_ip(DestIp) ->
  case get_dest_route(DestIp) of
    {If, ?NEXTHOP_DIRECT, _, _, _} ->
      <<N1, N2, N3, N4>> = <<DestIp:32>>,
      {If, {N1, N2, N3, N4}};
    {If, Nexthop, _, _, _} ->
      {If, Nexthop}
  end.

%%====================================================================
%% Internal functions
%%====================================================================

get_dest_route(DestIp) ->
  Routes = ets:match(routing_table, {'_', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11'}),
  Match = match_to_dest_ip(Routes, DestIp, []),
  fetch_to_dest_route(Match, {}).

fetch_to_dest_route([], NowRoute) ->
  NowRoute;

%% fetch to destination route
% directory connected
fetch_to_dest_route([{_, ?NEXTHOP_DIRECT, _, _, _}=Route| _], _) ->
  Route;
% ad 0
fetch_to_dest_route([{_, _, _, 0, _}=Route| _], _) ->
  Route;
% not set first routing
fetch_to_dest_route([Route| Tail], {}) ->
  fetch_to_dest_route(Tail, Route);
fetch_to_dest_route([{_, _, _, Ad, Metric}=Route| Tail], {_, _, _, Ad, NowMetric}=Now) ->
  if
    Metric > NowMetric ->
      fetch_to_dest_route(Tail, Now);
    true ->
      fetch_to_dest_route(Tail, Route)
  end;
fetch_to_dest_route([{_, _, Subnet, Ad, Metric}=Route| Tail], {_, _, Subnet, NowAd, NowMetric}=Now) ->
  if
    Ad > NowAd ->
      fetch_to_dest_route(Tail, Now);
    Ad =:= NowAd, Metric > NowMetric ->
      fetch_to_dest_route(Tail, Now);
    true ->
      fetch_to_dest_route(Tail, Route)
  end.

% match to destination ip
% match destination ip for routing table
match_to_dest_ip([], _, List) ->
  List;
match_to_dest_ip([[_, _, Ip, _, Subnetmask, Ad, Metric, Nexthop, _, If]| Tail], DestIp, List) when is_integer(DestIp) ->
  case DestIp band Subnetmask of
    Ip ->
      match_to_dest_ip(Tail, DestIp, [{If, Nexthop, Subnetmask, Ad, Metric}|List]);
    _ ->
      match_to_dest_ip(Tail, DestIp, List)
  end.

set_direct_routing_table([], List) ->
  List;
set_direct_routing_table([[_, _, ?SELEF_IP, _, _]| Tail], List) ->
  set_direct_routing_table(Tail, List);
set_direct_routing_table([[_, _, IP, Netmask, _]| Tail], List) when IP =:= undefined; Netmask =:= undefind ->
  set_direct_routing_table(Tail, List);
set_direct_routing_table([[_, Name, {I1, I2, I3, I4}, {S1, S2, S3, S4}, _]| Tail], List) ->
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

set_to_routing_table(#routing_table{dest_route=Ip, subnetmask=Netmask} = RoutingTable) when Ip =:= undefined; Netmask =:= undefined ->
  false;
set_to_routing_table(#routing_table{dest_route=Ip, subnetmask=Netmask} = RoutingTable) when is_tuple(Ip), is_tuple(Netmask) ->
  ets: insert_new(routing_table, RoutingTable).

