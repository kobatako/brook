%%%-------------------------------------------------------------------
%% @doc brook ethernet
%% @end
%%%-------------------------------------------------------------------

-module(brook_ethernet).

-export([receive_packet/1]).
-export([trance_to_tuple_mac_addr/1]).
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
  case brook_interface:match({interface, '$1', '$2', '$3', {S1, S2, S3, S4, S5, S6}, '_'}) of
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
  case brook_arp:get_mac_addr({IfName, NextIp}) of
    undefined ->
      brook_arp:request_arp(IfName, NextIp),
      gen_server:cast(brook_arp_pooling, {save_pooling, {Data, IfName, NextIp}}),
      false;
    DestMac when is_tuple(DestMac) ->
      packet_after_filter(Data, Opt#{dest_mac=>tuple_to_list(DestMac)});
    DestMac when is_list(DestMac) ->
      packet_after_filter(Data, Opt#{dest_mac=>DestMac})
  end.

%%--------------------------------------------------------------------
% trance mac addr
trance_to_tuple_mac_addr([D1, D2, D3, D4, D5, D6]) ->
  {D1, D2, D3, D4, D5, D6};
trance_to_tuple_mac_addr(<<D1, D2, D3, D4, D5, D6>>) ->
  {D1, D2, D3, D4, D5, D6};
trance_to_tuple_mac_addr(MacAddr) when is_integer(MacAddr) ->
  <<D1, D2, D3, D4, D5, D6>> = <<MacAddr:48>>,
  {D1, D2, D3, D4, D5, D6}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% packet after filter
%
packet_after_filter(Data, Opt) ->
  case brook_pipeline:after_ip_filter(Data, Opt) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, ResOpt} ->
      brook_sender:send_packet(ip_request, {Data, ResOpt})
  end.

%%--------------------------------------------------------------------
%
% IP Protocol
%
ethernet_type(?TYPE_IP, <<DestMacAddr:48, SourceMacAddr:48, Type:16, Data/bitstring>>) ->
  brook_arp:save_from_arp(<<DestMacAddr:48, SourceMacAddr:48, Type:16>>, Data),
  Opt = #{recv =>#{
          dest_mac_addr =>trance_to_tuple_mac_addr(DestMacAddr),
          source_mac_addr => trance_to_tuple_mac_addr(SourceMacAddr),
          type => Type
  }},
  case brook_pipeline:before_ip_filter(Data, Opt) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, Opt} ->
      apply(brook_ip, receive_packet, [Data, Opt])
  end;

%%--------------------------------------------------------------------
%
% ARP Protocol
%
ethernet_type(?TYPE_ARP, <<_:112, Data/bitstring>>) ->
  brook_arp:packet(Data);

%%--------------------------------------------------------------------
% not match Protocol
ethernet_type(_Type, _Data) ->
  undefined.

