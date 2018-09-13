%%%-------------------------------------------------------------------
%% @doc brook public API
%% @end
%%%-------------------------------------------------------------------

-module(brook_app).

-include("interface.hrl").
-behaviour(application).

-define(ETH_P_ALL, 16#0300).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, IF} = inet:getifaddrs(),
  Listen = lists:map(fun(Elm) -> interfaceList(Elm) end, IF),
  io:format("~p~n", [Listen]),
  Interface = ets:new(interface, [set, public, {keypos, #interface.name}, named_table]),
  saveInterface(Listen, Interface),
  {ok, FD} = procket:open(0, [
    {protocol, ?ETH_P_ALL},
    {type, raw},
    {family, packet}
  ]),
  arp:init(),
  ip:init(),
  brook_sup:start_link(),
  loop(FD).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

saveInterface([], _) ->
  true;
saveInterface([Head| Tail], Interface) ->
  ets:insert_new(Interface, Head),
  saveInterface(Tail, Interface).

%%
%% debug print interface
%%
printInterface(IF, Interface) ->
  case ets:next(Interface, IF) of
    '$end_of_table' ->
      true;
    Next ->
      printInterface(Next, Interface)
  end.

%
% interface list
%
interfaceList(Elm) ->
  {Name, Opts} = Elm,
  interfaceOpt(Opts, #interface{name=Name}).

%
%
%
interfaceOpt([], Opt) ->
  Opt;
interfaceOpt([Head| Tail], Opt) ->
  Res = case Head of
    {hwaddr, Hwaddr} ->
      Opt#interface{hwaddr=Hwaddr};
    {addr, Addr} ->
      Opt#interface{addr=Addr};
    {netmask, Netmask} ->
      Opt#interface{netmask=Netmask};
    _ ->
      Opt
  end,
  interfaceOpt(Tail, Res).

%
%
%
loop(FD) ->
  {Result, Buf} = procket:recvfrom(FD, 4096),
  if
    Result == error ->
      false;
    Result == ok ->
      <<DestMacAddr:48, SourceMacAddr:48, Type:16, Data/bitstring>> = Buf,
      <<S1, S2, S3, S4, S5, S6>> = <<SourceMacAddr:48>>,
      case ets:match(interface, {interface, '$1', '$2', '$3', [S1, S2, S3, S4, S5, S6]}) of
        [] ->
          io:format("Type ~p~n", [Type]),
          ethernetType(Type, Data);
        SelfIp ->
          source_my_ip
      end;
    true ->
      true
  end,
  loop(FD).

% ICMP Protocol
ethernetType(16#0800, Data) ->
  io:format("~p~n", ["icmp protocol"]),
  <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16,
      Flg:3, Offset:13, TTL, Protocol, CheckSum:16, SourceIp:32, DestIp:32,
      Type, Code, Other/bitstring>> = Data,

  Res = ets:match(interface, {interface, '$1', DestIp, '$2', '$3'}),
  io:format("~p~n", [Data]),
  io:format("~p~n", [Res]),
  io:format("Ver                   ~p~n", [Ver]),
  io:format("Len                   ~p~n", [Len]),
  io:format("ServiceType           ~p~n", [ServiceType]),
  io:format("Packetlen             ~p~n", [Packetlen]),
  io:format("IdentificationNumber  ~p~n", [IdentificationNumber]),
  io:format("Flg                   ~p~n", [Flg]),
  io:format("Offset                ~p~n", [Offset]),
  io:format("ttl                   ~p~n", [TTL]),
  io:format("protocol              ~p~n", [Protocol]),
  io:format("Dest                  ~p~n", [DestIp]),
  io:format("Dest                  ~p~n", [<<DestIp:32>>]),
  io:format("Source                ~p~n", [<<SourceIp:32>>]),
  io:format("Type                  ~p~n", [Type]),
  io:format("Code                  ~p~n", [Code]),
  {IfName, RouteIp} = ip:get_dest_ip(DestIp),
  case arp:get_mac_addr({IfName, RouteIp}) of
    false ->
      false;
    DestMac ->
      io:format("interface name ~p~n", [IfName]),
      io:format("dest mac addr ~p~n", [DestMac]),
      [[SourceMac]] = ets:match(interface, {'_', IfName, '_', '_', '$1'}),
      io:format("source mac addr ~p~n", [SourceMac]),
      %% TODO not count ttl
      CountTTL = TTL - 1,
      SendData = <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16,
          Flg:3, Offset:13, CountTTL, Protocol, CheckSum:16, SourceIp:32, DestIp:32,
          Type, Code, Other/bitstring>>,
      io:format("~p~n", [Data]),
      io:format("~p~n", [SendData]),
      sender:icmp_request(SourceMac, DestMac, IfName, Data)
  end,
  true;

% ARP Protocol
ethernetType(16#0806, Data) ->
  io:format("~p~n", ["arp protocol"]),
  arp:packet(Data),
  true;
ethernetType(_, _) ->
  true.
