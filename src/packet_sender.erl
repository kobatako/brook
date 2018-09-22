%%%-------------------------------------------------------------------
%% @doc brook sender
%% @end
%%%-------------------------------------------------------------------

-module(packet_sender).
-behaviour(gen_server).

% hardware type
-define(ETHERNET, 16#0001).
-define(ETH_P_IP, 16#0008).
-define(TYPE_ARP, 16#0806).
-define(TYPE_IP, 16#0800).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).
-export([terminate/2]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).

-record(ethernet_header, {
  source_mac_addr,
  dest_mac_addr,
  type
}).

-include("arp.hrl").

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

handle_cast({arp_request, {IfName, ArpData}}, State) ->
  #{ip_fd := FD, mac_addr := MacAddr} = get_interface_fd(State, IfName),
  arp_request(FD, MacAddr, ArpData),
  {noreply, State};

handle_cast({icmp_request, {IfName, DestMac, IpData}}, State) ->
  #{ip_fd := FD, mac_addr := SourceMac} = get_interface_fd(State, IfName),
  icmp_request(FD, SourceMac, DestMac, IpData),
  {noreply, State};

handle_cast({tcp_request, {IfName, DestMac, TcpData}}, State) ->
  #{ip_fd := FD, mac_addr := SourceMac} = get_interface_fd(State, IfName),
  tcp_request(FD, SourceMac, DestMac, TcpData),
  {noreply, State};

handle_cast({udp_request, {IfName, DestMac, TcpData}}, State) ->
  #{ip_fd := FD, mac_addr := SourceMac} = get_interface_fd(State, IfName),
  tcp_request(FD, SourceMac, DestMac, TcpData),
  {noreply, State};

handle_cast(_, State) ->
  {noreply, State}.

get_interface_fd([], _) ->
  false;
get_interface_fd([#{if_name := IfName}=IfFd| _], IfName) ->
  IfFd;
get_interface_fd([_| Tail], IfName) ->
  get_interface_fd(Tail, IfName).

% make bind file descriptor
make_bind([],  Res) ->
  Res;
make_bind([[interface, _, undefined, _, _]| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([[interface, _, _, undefined, _]| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([[interface, _, {127, 0, 0, 1}, _, _]| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([[_, IfName, {A1, A2, A3, A4}, _, MacAddr]|Tail], Res) ->
  {ok, IPFD} = procket:open(0, [
    {protocol, raw},
    {type, raw},
    {family, packet}
  ]),
  ok = packet:bind(IPFD, packet:ifindex(IPFD, IfName)),
  erlang:open_port({fd, IPFD, IPFD}, [binary, stream]),
  make_bind(Tail, [#{if_name => IfName, ip_fd => IPFD, mac_addr => MacAddr}| Res]).

%%
%% @doc request arp
%%
arp_request(FD, HwAddr, ARPHeader) ->
  Ethernet = ethernet_to_binary(#ethernet_header{source_mac_addr=HwAddr,
              dest_mac_addr=[16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff], type=?TYPE_ARP}),
  request(FD, <<Ethernet/bitstring, ARPHeader/bitstring>>).

%
% ip request
%
icmp_request(FD, SourceMac, DestMac, Data) when is_tuple(DestMac) ->
  icmp_request(FD, SourceMac, tuple_to_list(DestMac), Data);

icmp_request(FD, SourceMac, DestMac, Data) ->
  Ethernet = ethernet_to_binary(#ethernet_header{
                                  source_mac_addr=SourceMac,
                                  dest_mac_addr=DestMac,
                                  type=?TYPE_IP}
  ),
  request(FD, <<Ethernet/bitstring, Data/bitstring>>).

tcp_request(FD, SourceMac, DestMac, Data) when is_tuple(DestMac) ->
  tcp_request(FD, SourceMac, tuple_to_list(DestMac), Data);

tcp_request(FD, SourceMac, DestMac, Data) ->
  Ethernet = ethernet_to_binary(#ethernet_header{
                                  source_mac_addr=SourceMac,
                                  dest_mac_addr=DestMac,
                                  type=?TYPE_IP}
  ),
  request(FD, <<Ethernet/bitstring, Data/bitstring>>).


%
% request send packet
%
request(FD, Buf) ->
  case procket:write(FD, Buf) of
    ok ->
      true;
    {ok, S} ->
      true;
    {error, Er} ->
      io:format("send to error~n"),
      io:format("~p~n", [Er]),
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
