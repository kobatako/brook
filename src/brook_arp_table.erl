-module(brook_arp_table).

-include("arp.hrl").

-export([start_link/0]).
-export([table/0]).
-export([table/1]).
-export([save_arp_table/1]).
-export([fetch_dest_ip_addr/1]).
-export([fetch_dest_ip_addr/2]).
-export([fetch_dest_mac_addr/2]).

start_link() ->
  init().

init() ->
  {atomic, ok} = mnesia:create_table(arp_table, [{attributes, record_info(fields, arp_table)}]).

table() ->
  true.
table(show) ->
  mnesia:dirty_match_object(arp_table, {'_', '$1', '$2', '$3', '$4'}).

get_mac_addr({_, Nexthop}) ->
  case fetch_dest_ip_addr(Nexthop, false) of
    [] ->
      undefined;
    [{arp_table, _, _, DestMac, _}| _] ->
      DestMac;
    [DestMac| _] ->
      DestMac
  end.


%%--------------------------------------------------------------------
%
% fetch dest mac addr
%
fetch_dest_mac_addr(DestMac) ->
  fetch_dest_mac_addr(DestMac, true).

fetch_dest_mac_addr(DestMac, true) ->
  Func = fun() ->
    MatchHead = #arp_table{dest_mac_addr=DestMac, dest_ip_addr='$1', _='_'},
    Result = '$1',
    fetch_arp_table(MatchHead, Result)
  end,
  case mnesia:transaction(Func) of
    {atomic, Res} ->
      Res;
    {aborted, _} ->
      []
  end;

fetch_dest_mac_addr(DestMac, false) ->
  mnesia:dirty_match_object(arp_table, {'_', '$1', '$2', DestMac, '$3'}).

%%--------------------------------------------------------------------
%
% fetch dest ip addr
%
fetch_dest_ip_addr(Nexthop) ->
  fetch_dest_ip_addr(Nexthop, true).

fetch_dest_ip_addr(Nexthop, true) ->
  Func = fun() ->
    MatchHead = #arp_table{dest_ip_addr=Nexthop, dest_mac_addr='$1', _='_'},
    Result = '$1',
    fetch_arp_table(MatchHead, Result)
  end,
  case mnesia:transaction(Func) of
    {atomic, Res} ->
      Res;
    {aborted, _} ->
      []
  end;

fetch_dest_ip_addr(Nexthop, false) ->
  mnesia:dirty_match_object(arp_table, {'_', '$1', Nexthop, '$2', '$3'}).

%%--------------------------------------------------------------------
%
% fetch arp table
%
fetch_arp_table(MatchHead, Result) ->
    mnesia:select(arp_table, [{MatchHead, [], [Result]}]).


%%--------------------------------------------------------------------
%
% save arp table
%
save_arp_table(ArpTable) ->
  mnesia:transaction(fun() ->
    mnesia:write(arp_table, ArpTable, write)
  end).

