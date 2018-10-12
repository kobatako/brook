%%%-------------------------------------------------------------------
%% @doc brook IP
%% @end
%%%-------------------------------------------------------------------
-module(brook_tcp).

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
  case brook_pipeline:after_tcp_pipeline(Data, Opt) of
    {error, Msg} ->
      {error, Msg};
    {ok, ResData, ResOpt} ->
      brook_ip:send_packet(ResData, ResOpt)
  end.


