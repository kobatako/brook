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

-export([init/0, get_mac_addr/1, packet/1, to_binary/1]).

init() ->
  io:format("~p~n", ["arp init"]),
  arp_table = ets:new(arp_table, [set, protected, {keypos, #arp_table.dest_mac_addr}, named_table]).

get_mac_addr({If, Nexthop}) ->
  case ets:match(arp_table, {'_', '$1', Nexthop, '$2', '_'}) of
    [] ->
      request_arp(If, Nexthop),
      false;
    [[_, DestMac]] ->
      DestMac
  end.

%
% arp request
%
request_arp(If, Nexthop) ->
  case ets:match(interface, {'_', If, '$1', '_', '$2'}) of
    [] ->
      false;
    [[SourceIp, SourceMacAddr]] ->
      sender:arp_request(If, SourceMacAddr, SourceIp, Nexthop)
  end.

%
% arp response packet
%
packet(<<?ETHERNET:16, Type:16, _, _, ?OPTION_RESPONSE:16, SourceMacAddr:48, SourceIp:32, DestMacAddr:48, DestIp:32, _/bitstring>>) ->
  <<S1, S2, S3, S4>> = <<SourceIp:32>>,
  <<D1, D2, D3, D4>> = <<DestIp:32>>,
  <<SM1, SM2, SM3, SM4, SM5, SM6>> = <<SourceMacAddr:48>>,
  io:format("~p~n", ["arp response"]),
  io:format("type            ~p~n", [Type]),
  io:format("source mac addr ~p~n", [<<SourceMacAddr:48>>]),
  io:format("dest mac addr   ~p~n", [<<DestMacAddr:48>>]),
  io:format("Dest     ~p.~p.~p.~p~n", [D1, D2, D3, D4]),
  io:format("Source   ~p.~p.~p.~p~n", [S1, S2, S3, S4]),
  ets:insert_new(arp_table,
    #arp_table{source_ip_addr={D1, D2, D3, D4},
                dest_ip_addr={S1, S2, S3, S4},
                dest_mac_addr={SM1, SM2, SM3, SM4, SM5, SM6},
                type=?ETHERNET
    }
  );

packet(_) ->
  true.

to_binary(Record) ->
  HardwareType = binary:encode_unsigned(Record#arpHeader.hardwareType),
  Protocol = binary:encode_unsigned(Record#arpHeader.protocol),
  AddressLen = binary:encode_unsigned(Record#arpHeader.addressLen),
  ProtocolLen = binary:encode_unsigned(Record#arpHeader.protocolLen),
  OperationCode = binary:encode_unsigned(Record#arpHeader.operationCode),
  SourceMacAddress = list_to_binary(Record#arpHeader.sourceMacAddress),
  SourceIPAddress = list_to_binary(Record#arpHeader.sourceIPAddress),
  DestMacAddress = list_to_binary(Record#arpHeader.destMacAddress),
  DestIPAddress = list_to_binary(Record#arpHeader.destIPAddress),

  <<
    00,
    HardwareType/bitstring,
    Protocol/bitstring,
    AddressLen/bitstring,
    ProtocolLen/bitstring,
    00,
    OperationCode/bitstring,
    SourceMacAddress/bitstring,
    SourceIPAddress/bitstring,
    DestMacAddress/bitstring,
    DestIPAddress/bitstring
  >>.

