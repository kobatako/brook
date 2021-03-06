%%%-------------------------------------------------------------------
%% @doc brook arp
%% @end
%%%-------------------------------------------------------------------
-module(brook_arp).

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

-export([init/0]).
-export([table/0, table/1]).
-export([packet/1]).
-export([get_mac_addr/1]).
-export([save_from_arp/2]).
-export([request_arp/2]).

-type mac_address() :: {integer(), integer(), integer(), integer(), integer(), integer()}.
-export_type([mac_address/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% init
%
-spec init() -> {atomic, ok}.
init() ->
  brook_arp_table:start_link().

%%--------------------------------------------------------------------
%
% table
%
-spec table() -> true.
table() ->
  true.

%%--------------------------------------------------------------------
%
% table show arp table
%
-spec table(show) -> list().
table(Cont) ->
  brook_arp_table:table(Cont).

%%--------------------------------------------------------------------
%
% table show arp table
%
-spec get_mac_addr({brook_interface:name(), brook_ip:ip_address()}) -> mac_address() | undefined.
get_mac_addr({_, Nexthop}) ->
  case brook_arp_table:fetch_dest_ip_addr(Nexthop, false) of
    [] ->
      undefined;
    [{arp_table, _, _, DestMac, _}| _] ->
      DestMac;
    [DestMac| _] ->
      DestMac
  end.

%%--------------------------------------------------------------------
%
% arp response packet
%
-spec packet(bitstring()) ->  true | false.
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
  brook_arp_table:save_arp_table(ArpTable);
packet(_) ->
  true.

%%--------------------------------------------------------------------
%
% arp request
%
-spec request_arp(brook_interface:name(), brook_ip:ip_address()) -> false | ok.
request_arp(If, Nexthop) ->
  case brook_interface:match({'_', If, '$1', '_', '$2', '_'}) of
    [] ->
      false;
    [{_, _, SourceIp, _, SourceMacAddr, _}] ->
      ARPHeader = to_binary(#arp_header{
        hw_type=?ETHERNET, protocol=16#0800, address_len=16#06, protocol_len=16#04,
        operation_code=16#0001,
        source_mac_addr=tuple_to_list(SourceMacAddr), source_ip_addr=tuple_to_list(SourceIp),
        dest_mac_addr=[16#00, 16#00, 16#00, 16#00, 16#00, 16#00],
        dest_ip_addr=tuple_to_list(Nexthop)
      }),
      brook_sender:send_packet(arp_request, {ARPHeader, If})
  end.

%%--------------------------------------------------------------------
%
% save from arp
% save the source ip as ip protocol and direct connected network interface
%
-spec save_from_arp(bitstring(), bitstring()) -> not_match | true | false.
save_from_arp(
  <<_:48, SM1, SM2, SM3, SM4, SM5, SM6, _/bitstring>>,
  <<_:96, SourceAddr:32, _/bitstring>>
) ->
  case brook_arp_table:fetch_dest_mac_addr({SM1, SM2, SM3, SM4, SM5, SM6}, false) of
    [] ->
      case brook_interface:match_network(SourceAddr) of
        not_match ->
          not_save;
        #{addr:=Addr} ->
          <<S1, S2, S3, S4>> = <<SourceAddr:32>>,
          ArpTable = #arp_table{source_ip_addr=Addr,
              dest_ip_addr={S1, S2, S3, S4},
              dest_mac_addr={SM1, SM2, SM3, SM4, SM5, SM6},
              type=?ETHERNET
          },
          brook_arp_table:save_arp_table(ArpTable)
      end;
    _ ->
      true
  end;
save_from_arp(_, _) ->
  false.

%%====================================================================
%% Internal functions
%%====================================================================

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
