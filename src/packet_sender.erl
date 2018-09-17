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
-define(TYPE_ICMP, 16#0800).

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

start_link([]) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  Interfaces = ets:match(interface, {'$1', '$2', '$3', '$4', '$5'}),
  InterfaceFD = make_bind(Interfaces, []),
  {ok, InterfaceFD}.

handle_info(_Message, Storage) ->
  {noreply, Storage}.

terminate(_message, _Storage) ->
  ok.

handle_call(?MODULE, _, _) ->
  true.

handle_cast({arp_request, {IfName, ArpData}}, State) ->
  #{fd := FD, mac_addr := MacAddr} = get_interface_fd(State, IfName),
  arp_request(FD, MacAddr, ArpData),
  {noreply, State};

handle_cast({icmp_request, {IfName, DestMac, IcmpData}}, State) ->
  #{fd := FD, mac_addr := SourceMac} = get_interface_fd(State, IfName),
  icmp_request(FD, SourceMac, DestMac, IcmpData),
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
make_bind([], Res) ->
  Res;
make_bind([[interface, _, undefined, _, _]| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([[interface, _, _, undefined, _]| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([[interface, _, {127, 0, 0, 1}, _, _]| Tail], Res) ->
  make_bind(Tail, Res);
make_bind([[_, IfName, _, _, MacAddr]|Tail], Res) ->
  {ok, FD} = procket:open(0, [
    {protocol, ?ETH_P_IP},
    {type, raw},
    {family, packet},
    {interface, IfName}
  ]),
  ok = packet:bind(FD, packet:ifindex(FD,IfName)),
  erlang:open_port({fd, FD, FD}, [binary, stream]),
  make_bind(Tail, [#{if_name => IfName, fd => FD, mac_addr => MacAddr}| Res]).

%%
%% @doc request arp
%%
arp_request(FD, HwAddr, ARPHeader) ->
  Ethernet = ethernet_to_binary(#ethernet_header{source_mac_addr=HwAddr, dest_mac_addr=[16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff], type=?TYPE_ARP}),
  request(FD, <<Ethernet/bitstring, ARPHeader/bitstring>>).

%
% icmp request
%
icmp_request(FD, SourceMac, DestMac, Data) when is_tuple(DestMac) ->
  icmp_request(FD, SourceMac, tuple_to_list(DestMac), Data);

icmp_request(FD, SourceMac, DestMac, Data) ->
  Ethernet = ethernet_to_binary(#ethernet_header{
                                  source_mac_addr=SourceMac,
                                  dest_mac_addr=DestMac,
                                  type=?TYPE_ICMP}
  ),
  request(FD, <<Ethernet/bitstring, Data/bitstring>>).

%
% request send packet
%
request(FD, Buf) ->
  case procket:sendto(FD, Buf) of
    ok ->
      true;
    {ok, _} ->
      true;
    {_, _} ->
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
