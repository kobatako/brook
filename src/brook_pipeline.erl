%%%-------------------------------------------------------------------
%% @doc brook ethernet
%% @end
%%%-------------------------------------------------------------------

-module(brook_pipeline).
-export([init/0]).
-export([before_ip_filter/2]).
-export([after_ip_filter/2]).
-export([after_send_packet/2]).

-export([save_before_ip_filter/1]).
-export([save_before_ip_filter/2]).

-export([save_after_ip_filter/1]).
-export([save_after_ip_filter/2]).

-export([save_after_send_packet/1]).
-export([save_after_send_packet/2]).

-define(BEFORE_IP, before_ip_filter).
-define(AFTER_IP, after_ip_filter).
-define(AFTER_SEND_PACKET, after_send_packet).

-record(pipeline, {
  type,
  module,
  func
}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% init
%
init() ->
  mnesia:create_table(pipeline, [
    {attributes, record_info(fields, pipeline)},
    {type, bag}
  ]).

%%--------------------------------------------------------------------
%
% save before ip filter
%
save_before_ip_filter(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_IP, module=undefined, func=Func}, write)
  end).

save_before_ip_filter(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_IP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save after ip filter
%
save_after_ip_filter(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_IP, module=undefined, func=Func}, write)
  end).

save_after_ip_filter(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_IP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save after send packet
%
save_after_send_packet(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_SEND_PACKET, module=undefined, func=Func}, write)
  end).

save_after_send_packet(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_SEND_PACKET, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% before ip filter
%
before_ip_filter(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?BEFORE_IP, '$1', '$2'}
  ),
  filter(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% after ip filter
%
after_ip_filter(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?AFTER_IP, '$1', '$2'}
  ),
  filter(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% after send packet
%
after_send_packet(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?AFTER_SEND_PACKET, '$1', '$2'}
  ),
  filter(Filter, Data, Opt).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% filter
%
filter([], Data, Opt) ->
  {ok, Data, Opt};
filter([#pipeline{module=undefined, func=Func}| Tail], Data, Opt) ->
  case apply(Func, [Data, Opt]) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, Opt} ->
      filter(Tail, Data, Opt)
  end;
filter([#pipeline{module=Module, func=Func}| Tail], Data, Opt) ->
  case apply(Module, Func, [Data, Opt]) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, Opt} ->
      filter(Tail, Data, Opt)
  end.

