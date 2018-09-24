%%%-------------------------------------------------------------------
%% @doc brook sender
%% @end
%%%-------------------------------------------------------------------

-module(packet_sender).
-behaviour(gen_server).

-include("arp.hrl").

% hardware type
-define(ETHERNET, 16#0001).
-define(ETH_P_IP, 16#0008).
-define(TYPE_ARP, 16#0806).
-define(TYPE_IP, 16#0800).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).
-export([send_packet/2]).
-export([terminate/2]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).

-record(ethernet_header, {
  source_mac_addr,
  dest_mac_addr,
  type
}).

%%====================================================================
%% API
%%====================================================================

start_link([FD]) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [FD], []).

init([FD]) ->
  {ok, FD}.

handle_info(_Message, Storage) ->
  {noreply, Storage}.

terminate(_Message, _Storage) ->
  ok.

handle_call(?MODULE, _, _) ->
  true.

send_packet(ip_request, SendData) ->
  gen_server:cast(packet_sender, {ip_request, SendData});

send_packet(arp_request, SendData) ->
  gen_server:cast(packet_sender, {arp_request, SendData}).

handle_cast({arp_request, {IfName, ArpData}}, State) ->
  #{ip_fd := FD, mac_addr := MacAddr} = get_interface_fd(State, IfName),
  arp_request(FD, MacAddr, ArpData),
  {noreply, State};

handle_cast({ip_request, {IfName, DestMac, IpData}}, State) ->
  #{ip_fd := FD, mac_addr := SourceMac} = get_interface_fd(State, IfName),
  ip_request(FD, SourceMac, DestMac, IpData),
  {noreply, State};

handle_cast(_, State) ->
  {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% get interface fd
%
get_interface_fd([], _) ->
  false;
get_interface_fd([#{if_name := IfName}=IfFd| _], IfName) ->
  IfFd;
get_interface_fd([_| Tail], IfName) ->
  get_interface_fd(Tail, IfName).

%%--------------------------------------------------------------------
%%
%% @doc request arp
%%
arp_request(FD, HwAddr, ARPHeader) ->
  Ethernet = ethernet_to_binary(#ethernet_header{source_mac_addr=HwAddr,
              dest_mac_addr=[16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff], type=?TYPE_ARP}),
  request(FD, <<Ethernet/bitstring, ARPHeader/bitstring>>).

%%--------------------------------------------------------------------
%
% ip request
%
ip_request(FD, SourceMac, DestMac, Data) ->
  Ethernet = ethernet_to_binary(#ethernet_header{
                                  source_mac_addr=SourceMac,
                                  dest_mac_addr=DestMac,
                                  type=?TYPE_IP}
  ),
  request(FD, <<Ethernet/bitstring, Data/bitstring>>).

%%--------------------------------------------------------------------
%
% request send packet
%
request(FD, Buf) ->
  case procket:write(FD, Buf) of
    ok ->
      true;
    {ok, _} ->
      true;
    {error, _} ->
      false
  end.

% record convert to binary
ethernet_to_binary(Record) ->
  DestMacAddr = list_to_binary(Record#ethernet_header.dest_mac_addr),
  SourceMacAddr = list_to_binary(Record#ethernet_header.source_mac_addr),
  Type = binary:encode_unsigned(Record#ethernet_header.type),
  <<
    DestMacAddr/bitstring,
    SourceMacAddr/bitstring,
    Type/bitstring
  >>.
