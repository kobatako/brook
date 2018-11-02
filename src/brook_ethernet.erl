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
  Source = trance_to_tuple_mac_addr(SourceMacAddr),
  % where send packet
  % Sender himself
  case brook_interface:match_mac_addr(Source) of
    [] ->
      next_layer(Type, Buf);
    _ ->
      {error, receive_packet_source_self_mac_address, {source_mac_addr, Source}}
  end.

%%--------------------------------------------------------------------
%
% send packet
%
send_packet(Data, #{if_name:=IfName, next_ip:=NextIp}=Opt) ->
  case brook_arp:get_mac_addr({IfName, NextIp}) of
    undefined ->
      brook_arp:request_arp(IfName, NextIp),
      brook_arp_pooling:save_pooling(Data, IfName, NextIp),
      {ok, undefined_arp_table, {{next_ip, NextIp}, {if_name, IfName}}};
    DestMac when is_tuple(DestMac) ->
      {ok, ethernet_send_packet, {data, Data}, {opt, Opt#{dest_mac=>tuple_to_list(DestMac)}}};
    DestMac when is_list(DestMac) ->
      {ok, ethernet_send_packet, {data, Data}, {opt, Opt#{dest_mac=>DestMac}}}
  end;
send_packet(Data, Opt) ->
  {error, not_found_if_name_or_next_ip, {opt, Opt}}.

%%--------------------------------------------------------------------
%
% trance mac addr
%
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
% IP Protocol
%
next_layer(?TYPE_IP, <<EthernetData:112, Data1/bitstring>>) ->
  brook_arp:save_from_arp(<<EthernetData:112>>, Data1),
  Opt1 = make_pipeline_option(<<EthernetData:112>>),
  case brook_pipeline:before_ip_pipeline(Data1, Opt1) of
    {error, Msg} ->
      {error, ethernet_before_ip_pipeline, Msg};
    {ok, Data, Opt} ->
      case brook_ip:receive_packet(Data, Opt) of
        {ok, _, {data, Data0}, {opt, Opt0}} ->
          send_packet(Data0, Opt0);
        {error, _, _} = Err ->
          Err
      end
  end;

%%--------------------------------------------------------------------
%
% ARP Protocol
%
next_layer(?TYPE_ARP, <<_:112, Data/bitstring>>) ->
  brook_arp:packet(Data);

%%--------------------------------------------------------------------
%
% not match Protocol
%
next_layer(Type, _Data) ->
  {error, ethernet_undefined_type, {type, Type}}.

%%--------------------------------------------------------------------
%
% make pipeline option
%
make_pipeline_option(<<DestMacAddr:48, SourceMacAddr:48, Type:16>>) ->
  #{recv =>#{
    dest_mac_addr =>trance_to_tuple_mac_addr(DestMacAddr),
    source_mac_addr => trance_to_tuple_mac_addr(SourceMacAddr),
    type => Type
  }}.

