%%%-------------------------------------------------------------------
%% @doc brook IP
%% @end
%%%-------------------------------------------------------------------
-module(brook_ip).

-include("interface.hrl").
-include("ip.hrl").

-export([init/0]).
-export([route/1, route/3, route/2, route/6]).
-export([receive_packet/2]).
-export([send_packet/2]).
-export([trance_to_integer_ip_addr/1]).
-export([trance_to_tuple_ip_addr/1]).

-type ip_address() :: {integer(), integer(), integer(), integer()}.
-export_type([ip_address/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% init
%
init() ->
  brook_routing_table:start_link().

%%--------------------------------------------------------------------
%
% receive packet
%
receive_packet(
    <<_:4, Len:4, _:72, CheckSum:16, _:?IP_LEN, DestIp:?IP_LEN, _/bitstring>> = Data,
    Opt
) ->
  case check_sum_header(Len, Data) of
    CheckSum ->
      Dest = trance_to_tuple_ip_addr(<<DestIp:?IP_LEN>>),
      case brook_interface:match_ip_addr(Dest) of
        [] ->
          receive_next_layer(Data, Opt);
        Res ->
          {error, match_to_ip_addr, {interface, Res}}
      end;
    _ ->
      {error, response_packet_not_check_sum, {header_chech_sum, CheckSum}}
  end.

check_sum_header(HeadLen, <<Head:80, _:16, Other/bitstring>>) ->
  Len = HeadLen * 4 * 8 - 96,
  <<Food:Len, _/bitstring>> = Other,
  CheckData = <<Head:80, Food:Len>>,
  checksum(CheckData, 16#0000).

%%--------------------------------------------------------------------
%
% send packet
%
send_packet(<<_:72, 0, _Other/bitstring>>, _) ->
  {error, not_send_packet, {}};
send_packet(<<Ver:4, HeadLen:4, Head:72, _:16, SourceIp:32, DestIp:32, Other/bitstring>>, Opt) ->
  Len = HeadLen * 4 * 8 - 96 - 64,
  <<Food:Len, _/bitstring>> = Other,
  SendData = <<Ver:4, HeadLen:4, Head:72, SourceIp:?IP_LEN, DestIp:?IP_LEN, Food:Len>>,
  SendCheckSum = checksum(SendData, 16#0000),
  case get_dest_ip(DestIp) of
    not_found_dest_ip ->
      {error, not_found_dest_ip, {dest_ip, DestIp}};
    {IfName, NextIp} ->
      packet_after_filter(
        <<
          Ver:4, HeadLen:4, Head:72, SendCheckSum:16, SourceIp:?IP_LEN,
          DestIp:?IP_LEN, Other/bitstring
        >>,
        Opt#{if_name=>IfName, next_ip=>NextIp}
      )
  end.

%%--------------------------------------------------------------------
% show route
-spec route(atom()) -> list().
route(show) ->
  brook_routing_table:all_routing_table();

route(_) ->
  {error, not_match_control}.

%%--------------------------------------------------------------------
% show route, source route
-spec route(atom(), atom()) -> list().
% show route, source static
route(show, static) ->
  brook_routing_table:routing_table(?SOURCE_STATIC);
route(show, ?SOURCE_STATIC) ->
  brook_routing_table:routing_table(?SOURCE_STATIC);

% show route, source connect
route(show, connect) ->
  brook_routing_table:routing_table(?SOURCE_DIRECT);
route(show, ?SOURCE_DIRECT) ->
  brook_routing_table:routing_table(?SOURCE_DIRECT);

route(_, _) ->
  {error, not_match_control}.

%%--------------------------------------------------------------------
% add static route
-spec route(atom(), atom(), map()) -> atom() | tuple().
route(add, static, #{dest_route := {D1, D2, D3, D4}, subnetmask := {S1, S2, S3, S4},
      nexthop := {_, _, _, _}=Nexthop, out_interface := OutInterface}) ->
  <<DestRoute:?IP_LEN>> = <<D1, D2, D3, D4>>,
  <<Subnetmask:?IP_LEN>> = <<S1, S2, S3, S4>>,
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
  brook_routing_table:write_routing_table(RoutingTable),
  ok;

route(add, static, _) ->
  {error, not_match_add_static_route};

route(add, _, _) ->
  {error, not_match_static_route};

route(_, _, _) ->
  {error, not_match_control}.


% add static route
-spec route(atom(), atom(), tuple(), tuple(), tuple(), string()) -> atom() | tuple().
route(add, static, {D1, D2, D3, D4}, {S1, S2, S3, S4}, {_, _, _, _}=Nexthop, OutInterface) ->
  <<DestRoute:?IP_LEN>> = <<D1, D2, D3, D4>>,
  <<Subnetmask:?IP_LEN>> = <<S1, S2, S3, S4>>,
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
  brook_routing_table:write_routing_table(RoutingTable),
  ok;

route(add, static, _, _, _, _) ->
  {error, not_match_add_static_route};

route(add, _, _, _, _, _) ->
  {error, not_match_static_route};

route(_, _, _, _, _, _) ->
  {error, not_match_control}.

%%--------------------------------------------------------------------
%
% trance to integer ip addr
%
trance_to_integer_ip_addr({I1, I2, I3, I4}) ->
  <<Ip:?IP_LEN>> =  <<I1, I2, I3, I4>>,
  Ip;
trance_to_integer_ip_addr(_) ->
  undefined.

%%--------------------------------------------------------------------
%
% trance to tuple ip addr
%
trance_to_tuple_ip_addr(<<I1, I2, I3, I4>>) ->
  {I1, I2, I3, I4};
trance_to_tuple_ip_addr(Ip) when is_integer(Ip) ->
  <<I1, I2, I3, I4>> = <<Ip:?IP_LEN>>,
  {I1, I2, I3, I4};
trance_to_tuple_ip_addr(_) ->
  undefined.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% packet after filter
%
packet_after_filter(Data, Opt) ->
  case brook_pipeline:after_ip_pipeline(Data, Opt) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, ResOpt} ->
      {ok, packet_after_filter, {data, Data}, {opt, ResOpt}}
  end.

% other ip
% icmp protocol
receive_next_layer(<<Head:64, TTL, ?ICMP_PROTOCOL, Other/bitstring>>, Opt0) ->
  case brook_icmp:receive_packet(<<Head:64, (TTL-1), 1, Other/bitstring>>, Opt0) of
    {ok, _, {data, Data}, {opt, Opt}} ->
      brook_ip:send_packet(Data, Opt);
    {error, _, _} = Err ->
      Err
  end;

receive_next_layer(<<Head:64, TTL, ?TCP_PROTOCOL, Other/bitstring>>, Opt) ->
  case brook_pipeline:before_tcp_pipeline(
        <<Head:64, (TTL-1), ?TCP_PROTOCOL, Other/bitstring>>,
        Opt
      ) of
    {error, Msg} ->
      {error, before_tcp_pipeline, Msg};
    {ok, ResData0, ResOpt0} ->
      case brook_tcp:receive_packet(ResData0, ResOpt0) of
        {ok, _, {data, ResData}, {opt, ResOpt}} ->
          send_packet(ResData, ResOpt);
        {error, _, _} = Err ->
          Err
      end
  end;

receive_next_layer(<<Head:64, TTL, ?UDP_PROTOCOL, Other/bitstring>>, Opt) ->
  case brook_pipeline:before_udp_pipeline(
          <<Head:64, (TTL-1), ?UDP_PROTOCOL, Other/bitstring>>,
          Opt
        ) of
    {error, Msg} ->
      {error, before_udp_pipeline, Msg};
    {ok, ResData, ResOpt} ->
      brook_udp:receive_packet(ResData, ResOpt)
  end;

% other protocol
receive_next_layer(<<Head:64, TTL, Other/bitstring>>, Opt) ->
  send_packet(<<Head:64, (TTL-1), Other/bitstring>>, Opt).

%%--------------------------------------------------------------------
%
% get destination ip
%
get_dest_ip(DestIp) ->
  case get_dest_route(DestIp) of
    {} ->
      not_found_dest_ip;
    {If, ?NEXTHOP_DIRECT, _, _, _} ->
      {If, trance_to_tuple_ip_addr(DestIp)};
    {If, Nexthop, _, _, _} ->
      {If, Nexthop}
  end.

%%--------------------------------------------------------------------
% IP check sump
checksum(<<>>, Sum) ->
  Sum bxor 16#FFFF;
checksum(<<A:16, Other/bitstring>>, Sum) ->
  Check = A + Sum,
  Res = (Check band 16#FFFF) + (Check bsr 16),
  checksum(Other, Res);
checksum(<<_>>, _) ->
  bad_match_data_len.

%%--------------------------------------------------------------------
get_dest_route(DestIp) ->
  Routes = brook_routing_table:all_routing_table(),
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

