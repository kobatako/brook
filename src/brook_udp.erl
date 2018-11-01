%%%-------------------------------------------------------------------
%% @doc brook IP
%% @end
%%%-------------------------------------------------------------------
-module(brook_udp).

-export([receive_packet/2]).
-export([send_packet/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% recieve_packet
%
receive_packet(Data, Opt) ->
  send_packet(Data, Opt).

%%--------------------------------------------------------------------
%
% send_packet
%
send_packet(Data, Opt) ->
  case brook_pipeline:after_udp_pipeline(Data, Opt) of
    {error, Msg} ->
      {error, Msg};
    {ok, ResData, ResOpt} ->
      {ok, after_udp_pipeline, {data, ResData}, {opt, ResOpt}}
  end.

