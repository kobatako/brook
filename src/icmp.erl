-module(icmp).

-export([packet/1]).

packet(Data) ->
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


  end.

% ICMP check sump
checksum(<<>>, Sum) ->
  Sum bxor 16#FFFF;
checksum(<<A:16, Other/bitstring>>, Sum) ->
  Check = A + Sum,
  Res = (Check band 16#FFFF) + (Check bsr 16),
  checksum(Other, Res).

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

