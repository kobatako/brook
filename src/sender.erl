%%%-------------------------------------------------------------------
%% @doc brook sender
%% @end
%%%-------------------------------------------------------------------

-module(sender).

% hardware type
-define(ETHERNET, 16#0001).
-define(ETH_P_IP, 16#0008).
-define(TYPE_ARP, 16#0806).
-define(TYPE_ICMP, 16#0800).

-export([arp_request/4, icmp_request/4]).

-record(ethernetHeader, {
  sourceMacAddress,
  destMacAddress,
  type
}).

-include("arp.hrl").

%%
%% @doc request arp
%%
arp_request(IfName, HwAddr, Addr, DestIpAddr) ->
  io:format("arp request~n"),
  Ethernet = ethernet_to_binary(#ethernetHeader{sourceMacAddress=HwAddr, destMacAddress=[16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff], type=?TYPE_ARP}),
  ARPHeader = arp:to_binary(#arpHeader{
    hardwareType=?ETHERNET, protocol=16#0800, addressLen=16#06, protocolLen=16#04,
    operationCode=16#0001,
    sourceMacAddress=HwAddr, sourceIPAddress=tuple_to_list(Addr),
    destMacAddress=[16#00, 16#00, 16#00, 16#00, 16#00, 16#00], destIPAddress=tuple_to_list(DestIpAddr)
  }),
  {ok, FD} = procket:open(0, [
    {protocol, ?ETH_P_IP},
    {type, raw},
    {family, packet},
    {interface, IfName}
  ]),
  ok = packet:bind(FD, packet:ifindex(FD,IfName)),
  erlang:open_port({fd, FD, FD}, [binary, stream]),
  Buf = <<Ethernet/bitstring, ARPHeader/bitstring>>,
  io:format(" ---- send buf : ~w~n", [Buf]),
  case procket:sendto(FD, Buf) of
    ok ->
      io:format("send : ok~n");
    {ok, Size} ->
      io:format("send size to : ~w~n", [Size]);
    {Case, Other} ->
      io:format("send to : ~w : ~w~n", [Case, Other])
  end.

icmp_request(SourceMac, DestMac, IfName, Data) when is_tuple(DestMac) ->
  icmp_request(SourceMac, tuple_to_list(DestMac), IfName, Data);
icmp_request(SourceMac, DestMac, IfName, Data) ->
  io:format("icmp request~n"),
  Ethernet = ethernet_to_binary(#ethernetHeader{sourceMacAddress=SourceMac, destMacAddress=DestMac, type=?TYPE_ICMP}),
  {ok, FD} = procket:open(0, [
    {protocol, ?ETH_P_IP},
    {type, raw},
    {family, packet},
    {interface, IfName}
  ]),
  ok = packet:bind(FD, packet:ifindex(FD,IfName)),
  erlang:open_port({fd, FD, FD}, [binary, stream]),
  Buf = <<Ethernet/bitstring, Data/bitstring>>,
  io:format(" ---- send buf : ~w~n", [Buf]),
  case procket:sendto(FD, Buf) of
    ok ->
      io:format("send : ok~n");
    {ok, Size} ->
      io:format("send size to : ~w~n", [Size]);
    {Case, Other} ->
      io:format("send to : ~w : ~w~n", [Case, Other])
  end.

% record convert to binary
ethernet_to_binary(Record) ->
  DestMacAddress = list_to_binary(Record#ethernetHeader.destMacAddress),
  SourceMacAddress = list_to_binary(Record#ethernetHeader.sourceMacAddress),
  Type = binary:encode_unsigned(Record#ethernetHeader.type),
  <<
    DestMacAddress/bitstring,
    SourceMacAddress/bitstring,
    Type/bitstring
  >>.
