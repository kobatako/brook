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

%%--------------------------------------------------------------------
%
% receive packet
%
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

%%--------------------------------------------------------------------
%
% send packet
%
send_packet(Data, #{if_name:=IfName, next_ip:=NextIp}=Opt) ->
  case arp:get_mac_addr({IfName, NextIp}) of
    undefined ->
      arp:request_arp(IfName, NextIp),
      gen_server:cast(arp_pooling, {save_pooling, {Data, IfName, NextIp}}),
      false;
    DestMac when is_tuple(DestMac) ->
      packet_after_filter(Data, Opt#{dest_mac=>tuple_to_list(DestMac)});
    DestMac when is_list(DestMac) ->
      packet_after_filter(Data, Opt#{dest_mac=>DestMac})
  end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% packet after filter
%
packet_after_filter(Data, Opt) ->
  case pipeline:after_ip_filter(Data, Opt) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, ResOpt} ->
      packet_sender:send_packet(ip_request, {Data, ResOpt})
  end.

%%--------------------------------------------------------------------
%
% IP Protocol
%
ethernet_type(?TYPE_IP, <<Ether:112, Data/bitstring>>) ->
  arp:save_from_arp(<<Ether:112>>, Data),
  case pipeline:before_ip_filter(Data) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, Opt} ->
      apply(ip, receive_packet, [Data, Opt])
  end;

%%--------------------------------------------------------------------
%
% ARP Protocol
%
ethernet_type(?TYPE_ARP, <<_:112, Data/bitstring>>) ->
  arp:packet(Data);

%%--------------------------------------------------------------------
% not match Protocol
ethernet_type(_Type, _Data) ->
  undefined.

