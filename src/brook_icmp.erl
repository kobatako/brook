-module(brook_icmp).

-define(TYPE_TIME_EXCEEDED, 11).

-export([receive_packet/2]).

% time out exceeded
receive_packet(
  <<Head:64, TTL, 1, Checksum:16, SourceIp:32, _Other/bitstring>>,
  Opt
) when TTL =< 0 ->
  {error, icmp_packet_ttl, {}};

receive_packet(<<Data/bitstring>>, Opt) ->
  {ok, receive_packet, {data, Data}, {opt, Opt}}.


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
