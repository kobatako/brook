%%%-------------------------------------------------------------------
%% @doc brook IP
%% @end
%%%-------------------------------------------------------------------
-module(brook_routing_table).

-include("interface.hrl").
-include("ip.hrl").


-export([start_link/0]).
-export([write_routing_table/1]).
-export([routing_table/1]).
-export([all_routing_table/0]).
-export([set_direct_routing_table/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%
% write routing table
%
start_link() ->
  init().

%%--------------------------------------------------------------------
%
% write routing table
%
init() ->
  {atomic, ok} = mnesia:create_table(routing_table, [
    {attributes, record_info(fields, routing_table)},
    {type, bag}
  ]),
  IfList = brook_interface:list(),
  set_direct_routing_table(IfList, []).


%%--------------------------------------------------------------------
%
% write routing table
%
write_routing_table(RoutingTable) ->
  mnesia:transaction(fun() ->
    mnesia:write(routing_table, RoutingTable, write)
  end).

%%--------------------------------------------------------------------
%
%  all routing table
%
all_routing_table() ->
  mnesia:dirty_match_object(routing_table,
    {'_', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11'}).

routing_table(SourceRoute) ->
  mnesia:dirty_match_object(routing_table,
    {'_', SourceRoute, '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11'}).

set_direct_routing_table([], List) ->
  List;
set_direct_routing_table([{_, _, ?SELEF_IP, _, _, _}| Tail], List) ->
  set_direct_routing_table(Tail, List);
set_direct_routing_table([{_, _, IP, Netmask, _, _}| Tail], List)
                        when IP =:= undefined; Netmask =:= undefind ->
  set_direct_routing_table(Tail, List);
set_direct_routing_table([{_, Name, Dest, Subnet, _, _}| Tail], List) ->
  DestIp = brook_ip:trance_to_integer_ip_addr(Dest),
  Subnetmask = brook_ip:trance_to_integer_ip_addr(Subnet),
  Mask = DestIp band Subnetmask,
  set_to_routing_table(#routing_table{
    source_route=?SOURCE_DIRECT,
    dest_route=brook_ip:trance_to_tuple_ip_addr(Mask),
    dest_route_int=Mask,
    subnetmask=Subnet,
    subnetmask_int=Subnetmask,
    ad=0,
    metric=0,
    nexthop=?NEXTHOP_DIRECT,
    age=0,
    out_interface=Name
  }),
  set_direct_routing_table(Tail, List).

%%====================================================================
%% Internal functions
%%====================================================================

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

