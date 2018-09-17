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
      gen_server:cast(packet_sender, {icmp_request,
        {IfName, DestMac, <<Ver:4, Len:4, ServiceType, Packetlen:16, IdentificationNumber:16, Flg:3,
                          Offset:13, CountTTL, Protocol, SendCheckSum:16, SourceIp:32, DestIp:32,
                          Type, Code, Other/bitstring>>}
        }
      )


  end.

% ICMP check sump
checksum(<<>>, Sum) ->
  Sum bxor 16#FFFF;
checksum(<<A:16, Other/bitstring>>, Sum) ->
  Check = A + Sum,
  Res = (Check band 16#FFFF) + (Check bsr 16),
  checksum(Other, Res).

