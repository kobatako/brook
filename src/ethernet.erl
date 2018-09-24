%%%-------------------------------------------------------------------
%% @doc brook ethernet
%% @end
%%%-------------------------------------------------------------------

-module(ethernet).

-export([receive_packet/1]).
-export([send_packet/2]).

-define(TYPE_ARP,  16#0806).
-define(TYPE_IP, 16#0800).

%%====================================================================
%% API
%%====================================================================

receive_packet(<<_:48, SourceMacAddr:48, Type:16, _/bitstring>>=Buf) ->
  <<S1, S2, S3, S4, S5, S6>> = <<SourceMacAddr:48>>,
  % where send packet
  % Sender himself
  case interface:match({interface, '$1', '$2', '$3', [S1, S2, S3, S4, S5, S6], '_'}) of
    [] ->
      ethernet_type(Type, Buf);
    _ ->
      source_self_mac_address
  end.

send_packet(Data, {IfName, NextIp}) ->
  case arp:get_mac_addr({IfName, NextIp}) of
    undefined ->
      arp:request_arp(IfName, NextIp),
      false;
    DestMac when is_tuple(DestMac) ->
      packet_sender:send_packet(ip_request, {IfName, tuple_to_list(DestMac), Data});
    DestMac when is_list(DestMac) ->
      packet_sender:send_packet(ip_request, {IfName, DestMac, Data})
  end.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
% IP Protocol
ethernet_type(?TYPE_IP, <<Ether:112, Data/bitstring>>) ->
  arp:save_from_arp(<<Ether:112>>, Data),
  ip:receive_packet(Data);

%%--------------------------------------------------------------------
% ARP Protocol
ethernet_type(?TYPE_ARP, <<_:112, Data/bitstring>>) ->
  arp:packet(Data);

%%--------------------------------------------------------------------
% not match Protocol
ethernet_type(_Type, _Data) ->
  undefined.

