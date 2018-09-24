-module(icmp).

-export([receive_packet/1]).

% time out exceeded
receive_packet(<<_, TTL, 1, _:16, _SourceIp:32, _Other/bitstring>>) when TTL =< 0 ->
  false;

receive_packet(<<Data/bitstring>>) ->
  ip:send_packet(Data).

