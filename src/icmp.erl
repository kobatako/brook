-module(icmp).

-define(TYPE_TIME_EXCEEDED, 11).

-export([receive_packet/2]).

% time out exceeded
receive_packet(<<Head:64, TTL, 1, Checksum:16, SourceIp:32, _Other/bitstring>>, Opt) when TTL =< 0 ->
  TypeCode = <<?TYPE_TIME_EXCEEDED, 0>>,
  Checksum = checksum(TypeCode, 16#0000),
  io:format("icmp time exceeded~n"),
  io:format("~p~n", [Opt]),
  false;

receive_packet(<<Data/bitstring>>, Opt) ->
  ip:send_packet(Data, Opt).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
% IP check sump
checksum(<<>>, Sum) ->
  Sum bxor 16#FFFF;
checksum(<<A:16, Other/bitstring>>, Sum) ->
  Check = A + Sum,
  Res = (Check band 16#FFFF) + (Check bsr 16),
  checksum(Other, Res).
