-module(main).
-behaviour(supervisor).
-define(PROFILER_ON, false).

-ifdef (false).
-define(PROFILER_STOP(), fprof:trace([stop]), fprof:profile(), fprof:analyse([totals, {sort, own}, {dest, "fprof.analysis"}]), fprof:stop()).
-define(PROFILER_START(X), fprof:start(), fprof:trace([start, {procs, X}])).
-else.
-define(PROFILER_STOP(), []).
-define(PROFILER_START(X), []).
-endif.

-include("interface.hrl").

-export([start_link/1]).
-export([init/1]).
-define(ETH_P_ALL, 16#0300).

handle_call(_, _, _) ->
  true.
start_link(FD) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [FD]).

init([FD]) ->
  FId = spawn(fun() -> loop(FD) end),
  io:format("loop id : ~p~n", [FId]),
  {ok, { {one_for_one, 60, 3600}, []} }.
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
          ?PROFILER_START(self()),
          ethernetType(Type, Data),
          ?PROFILER_STOP();
        SelfIp ->
          source_my_ip
      end;
    true ->
      true
  end,
  loop(FD).

% ICMP Protocol
ethernetType(16#0800, Data) ->
  <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16,
      Flg:3, Offset:13, TTL, Protocol, CheckSum:16, SourceIp:32, DestIp:32,
      Type, Code, Other/bitstring>> = Data,
  {IfName, RouteIp} = ip:get_dest_ip(DestIp),
  case arp:get_mac_addr({IfName, RouteIp}) of
    false ->
      false;
    DestMac ->
      [[SourceMac]] = ets:match(interface, {'_', IfName, '_', '_', '$1'}),
      CountTTL = TTL - 1,
      SendData = <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16,
          Flg:3, Offset:13, CountTTL, Protocol, SourceIp:32, DestIp:32>>,
      SendCheckSum = checksum(SendData, 16#0000),
      gen_server:cast(packet_sender, {icmp_request, {IfName, DestMac, <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16,Flg:3, Offset:13, CountTTL, Protocol, SendCheckSum:16, SourceIp:32, DestIp:32, Type, Code, Other/bitstring>>}})

  end,
  true;

% ARP Protocol
ethernetType(16#0806, Data) ->
  arp:packet(Data),
  true;
ethernetType(_, _) ->
  true.

debug_icmp_print(Data) ->
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
  io:format("checksum              ~p~n", [CheckSum]),
  io:format("Dest                  ~p~n", [DestIp]),
  io:format("Dest                  ~p~n", [<<DestIp:32>>]),
  io:format("Source                ~p~n", [<<SourceIp:32>>]),
  io:format("Type                  ~p~n", [Type]),
  io:format("Code                  ~p~n", [Code]).

checksum(<<>>, Sum) ->
  Sum bxor 16#FFFF;
checksum(<<A:16, Other/bitstring>>, Sum) ->
  Check = A + Sum,
  Res = (Check band 16#FFFF) + (Check bsr 16),
  checksum(Other, Res).
