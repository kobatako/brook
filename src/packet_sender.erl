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
-export([handle_info/2]).
-export([handle_cast/2]).

-record(ethernetHeader, {
  sourceMacAddress,
  destMacAddress,
  type
}).

-include("arp.hrl").

start_link([]) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  io:format("send init ~n"),
  Interfaces = ets:match(interface, {'$1', '$2', '$3', '$4', '$5'}),
  InterfaceFD = make_bind(Interfaces, []),
  io:format("~p~n", [InterfaceFD]),
  {ok, InterfaceFD}.

handle_info(_Message, Storage) ->
  {noreply, Storage}.
terminate(_message, _Storage) ->
  ok.
handle_call(?MODULE, {pid, Tag}, State) ->
  true.

handle_cast({arp_request, {IfName, ArpData}}, State) ->
  #{fd := FD, mac_addr := MacAddr} = get_interface_fd(State, IfName),
  io:format("handle cast arp requst ~n"),
  io:format("~p ~n", [FD]),
  io:format("~p ~n", [MacAddr]),
  arp_request(FD, MacAddr, ArpData),
  {noreply, State};
handle_cast({icmp_request, {IfName, DestMac, IcmpData}}, State) ->
  #{fd := FD, mac_addr := SourceMac} = get_interface_fd(State, IfName),
  icmp_request(FD, SourceMac, DestMac, IcmpData),
  {noreply, State};
handle_cast(Message, State) ->
  io:format("~p~n", [Message]),
  io:format("~p~n", [State]),
  send(Message, State),
  {noreply, State}.
  % arp_request(FD, MacAddr, ArpData),

send({arp_request, {IfName, ArpData}}, State) ->
  #{fd := FD, mac_addr := MacAddr} = get_interface_fd(State, IfName),
  io:format("handle cast arp requst ~n"),
  io:format("~p ~n", [FD]),
  io:format("~p ~n", [MacAddr]),
  true.

get_interface_fd([], _) ->
  false;
get_interface_fd([#{if_name := IfName}=IfFd| Tail], IfName) ->
  IfFd;
get_interface_fd([Head| Tail], IfName) ->
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
make_bind([[_, IfName, IP, Subnet, MacAddr]|Tail], Res) ->
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
  Ethernet = ethernet_to_binary(#ethernetHeader{sourceMacAddress=HwAddr, destMacAddress=[16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff], type=?TYPE_ARP}),
  Buf = <<Ethernet/bitstring, ARPHeader/bitstring>>,
  case procket:sendto(FD, Buf) of
    ok ->
      true;
    {ok, Size} ->
      true;
    {Case, Other} ->
      false
  end.

icmp_request(FD, SourceMac, DestMac, Data) when is_tuple(DestMac) ->
  icmp_request(FD, SourceMac, tuple_to_list(DestMac), Data);
icmp_request(FD, SourceMac, DestMac, Data) ->
  Ethernet = ethernet_to_binary(#ethernetHeader{sourceMacAddress=SourceMac, destMacAddress=DestMac, type=?TYPE_ICMP}),
  Buf = <<Ethernet/bitstring, Data/bitstring>>,
  case procket:sendto(FD, Buf) of
    ok ->
      true;
    {ok, Size} ->
      true;
    {Case, Other} ->
      false
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
