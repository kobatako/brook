-module(icmp).

-export([receive_packet/2]).

% time out exceeded
receive_packet(<<_, TTL, 1, _:16, _SourceIp:32, _Other/bitstring>>, _) when TTL =< 0 ->
  false;

receive_packet(<<Data/bitstring>>, Opt) ->
  ip:send_packet(Data, Opt).

