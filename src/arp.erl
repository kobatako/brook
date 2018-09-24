%%%-------------------------------------------------------------------
%% @doc brook arp
%% @end
%%%-------------------------------------------------------------------
-module(arp).

% hardware type
-define(ETHERNET, 16#0001).

% protocol type
-define(PROTOCOL_IP, 16#0800).

% arp tale type
-define(DYNAMIC_TYPE, dynamic).
-define(STATIC_TYPE, static).

-include("arp.hrl").
% arp option
% response
-define(OPTION_RESPONSE, 16#0002).

-export([init/0, get_mac_addr/1, packet/1]).
-export([save_from_arp/2]).
-export([request_arp/2]).
-export([table/0, table/1]).

%%====================================================================
%% API
%%====================================================================

init() ->
  {atomic, ok} = mnesia:create_table(arp_table, [{attributes, record_info(fields, arp_table)}]).

table() ->
  true.
table(show) ->
  mnesia:dirty_match_object(arp_table, {'_', '$1', '$2', '$3', '$4'}).

get_mac_addr({If, Nexthop}) ->
  case mnesia:dirty_match_object(arp_table, {'_', '$1', Nexthop, '$2', '_'}) of
    [] ->
      undefined;
    [{arp_table, _, _, DestMac, _}| _] ->
      DestMac
  end.

%%--------------------------------------------------------------------
%
% arp response packet
%
packet(<<?ETHERNET:16, _:16, _, _, ?OPTION_RESPONSE:16,
          SourceMacAddr:48, SourceIp:32,
          _:48, DestIp:32, _/bitstring>>) ->
  <<S1, S2, S3, S4>> = <<SourceIp:32>>,
  <<D1, D2, D3, D4>> = <<DestIp:32>>,
  <<SM1, SM2, SM3, SM4, SM5, SM6>> = <<SourceMacAddr:48>>,
  ArpTable = #arp_table{source_ip_addr={D1, D2, D3, D4},
      dest_ip_addr={S1, S2, S3, S4},
      dest_mac_addr={SM1, SM2, SM3, SM4, SM5, SM6},
      type=?ETHERNET
  },
  mnesia:transaction(fun() ->
    mnesia:write(arp_table, ArpTable, write)
  end);
packet(_) ->
  true.

%%--------------------------------------------------------------------
%
% arp request
%
request_arp(If, Nexthop) ->
  case interface:match({'_', If, '$1', '_', '$2', '_'}) of
    [] ->
      false;
    [{_, _, SourceIp, _, SourceMacAddr, _}] ->
      ARPHeader = to_binary(#arp_header{
        hw_type=?ETHERNET, protocol=16#0800, address_len=16#06, protocol_len=16#04,
        operation_code=16#0001,
        source_mac_addr=SourceMacAddr, source_ip_addr=tuple_to_list(SourceIp),
        dest_mac_addr=[16#00, 16#00, 16#00, 16#00, 16#00, 16#00],
        dest_ip_addr=tuple_to_list(Nexthop)
      }),
      gen_server:cast(packet_sender, {arp_request, {If, ARPHeader}})
  end.

%%--------------------------------------------------------------------
%
% save from arp
% save the source ip as ip protocol and direct connected network interface
%
save_from_arp(
  <<_:48, SM1, SM2, SM3, SM4, SM5, SM6, _/bitstring>>,
  <<_:96, SourceAddr:32, _/bitstring>>
) ->
  case mnesia:dirty_match_object(arp_table,
    {'_', '$1', '$2', {SM1, SM2, SM3, SM4, SM5, SM6}, '_'}
  ) of
    [] ->
      case interface:match_network(SourceAddr) of
        not_match ->
          not_save;
        #{addr:=Addr} ->
          <<S1, S2, S3, S4>> = <<SourceAddr:32>>,
          ArpTable = #arp_table{source_ip_addr=Addr,
              dest_ip_addr={S1, S2, S3, S4},
              dest_mac_addr={SM1, SM2, SM3, SM4, SM5, SM6},
              type=?ETHERNET
          },
          io:format("save arp table: ~p~n", [ArpTable]),
          mnesia:transaction(fun() ->
            mnesia:write(arp_table, ArpTable, write)
          end)
      end;
    _ ->
      true
  end;
save_from_arp(_, _) ->
  false.

%%====================================================================
%% Internal functions
%%====================================================================

save_network_address_arp_table() ->

%%--------------------------------------------------------------------
%
% arp response packet
%
to_binary(Record) ->
  HwType = binary:encode_unsigned(Record#arp_header.hw_type),
  Protocol = binary:encode_unsigned(Record#arp_header.protocol),
  AddressLen = binary:encode_unsigned(Record#arp_header.address_len),
  ProtocolLen = binary:encode_unsigned(Record#arp_header.protocol_len),
  OperationCode = binary:encode_unsigned(Record#arp_header.operation_code),
  SourceMacAddr = list_to_binary(Record#arp_header.source_mac_addr),
  SourceIPAddr = list_to_binary(Record#arp_header.source_ip_addr),
  DestMacAddr = list_to_binary(Record#arp_header.dest_mac_addr),
  DestIPAddr = list_to_binary(Record#arp_header.dest_ip_addr),

  <<
    00,
    HwType/bitstring,
    Protocol/bitstring,
    AddressLen/bitstring,
    ProtocolLen/bitstring,
    00,
    OperationCode/bitstring,
    SourceMacAddr/bitstring,
    SourceIPAddr/bitstring,
    DestMacAddr/bitstring,
    DestIPAddr/bitstring
  >>.

