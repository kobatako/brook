%%%-------------------------------------------------------------------
%% @doc brook ethernet
%% @end
%%%-------------------------------------------------------------------

-module(brook_pipeline).
-export([init/0]).

%%--------------------------------------------------------------------
% call ip before and after pipeline
-export([before_ip_pipeline/2]).
-export([after_ip_pipeline/2]).

%%--------------------------------------------------------------------
% call tcp before and after pipeline
-export([before_tcp_pipeline/2]).
-export([after_tcp_pipeline/2]).

%%--------------------------------------------------------------------
% call udp before and after pipeline
-export([before_udp_pipeline/2]).
-export([after_udp_pipeline/2]).

%%--------------------------------------------------------------------
% call send after pipeline
-export([after_send_pipeline/2]).

%%--------------------------------------------------------------------
% save to ip before and after pipeline
-export([save_before_ip_pipeline/1, save_before_ip_pipeline/2]).
-export([save_after_ip_pipeline/1, save_after_ip_pipeline/2]).

%%--------------------------------------------------------------------
% save to tcp before and after pipeline
-export([save_before_tcp_pipeline/1, save_before_tcp_pipeline/2]).
-export([save_after_tcp_pipeline/1, save_after_tcp_pipeline/2]).

%%--------------------------------------------------------------------
% save to udp before and after pipeline
-export([save_before_udp_pipeline/1, save_before_udp_pipeline/2]).
-export([save_after_udp_pipeline/1, save_after_udp_pipeline/2]).

%%--------------------------------------------------------------------
% save to send after pipeline
-export([save_after_send_pipeline/1, save_after_send_pipeline/2]).

-define(BEFORE_IP, before_ip_pipeline).
-define(AFTER_IP, after_ip_pipeline).
-define(AFTER_SEND_PACKET, after_send_pipeline).
-define(AFTER_TCP, after_tcp_pipeline).
-define(BEFORE_TCP, before_tcp_pipeline).
-define(AFTER_UDP, after_udp_pipeline).
-define(BEFORE_UDP, before_udp_pipeline).

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
% save before ip pipeline
%
save_before_ip_pipeline(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_IP, module=undefined, func=Func}, write)
  end).

save_before_ip_pipeline(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_IP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save after ip pipeline
%
save_after_ip_pipeline(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_IP, module=undefined, func=Func}, write)
  end).

save_after_ip_pipeline(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_IP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save after send pipeline
%
save_after_send_pipeline(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_SEND_PACKET, module=undefined, func=Func}, write)
  end).

save_after_send_pipeline(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_SEND_PACKET, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save before tcp pipeline
%
save_before_tcp_pipeline(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_TCP, module=undefined, func=Func}, write)
  end).

save_before_tcp_pipeline(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_TCP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save after tcp pipeline
%
save_after_tcp_pipeline(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_TCP, module=undefined, func=Func}, write)
  end).

save_after_tcp_pipeline(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_TCP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save before udp pipeline
%
save_before_udp_pipeline(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_UDP, module=undefined, func=Func}, write)
  end).

save_before_udp_pipeline(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?BEFORE_UDP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% save after udp pipeline
%
save_after_udp_pipeline(Func) when is_function(Func, 2) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_UDP, module=undefined, func=Func}, write)
  end).

save_after_udp_pipeline(Module, Func) ->
  mnesia:transaction(fun() ->
    mnesia:write(pipeline, #pipeline{type=?AFTER_UDP, module=Module, func=Func}, write)
  end).

%%--------------------------------------------------------------------
%
% before ip pipeline
%
before_ip_pipeline(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?BEFORE_IP, '$1', '$2'}
  ),
  pipeline(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% after ip pipeline
%
after_ip_pipeline(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?AFTER_IP, '$1', '$2'}
  ),
  pipeline(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% after send pipeline
%
after_send_pipeline(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?AFTER_SEND_PACKET, '$1', '$2'}
  ),
  pipeline(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% before tcp pipeline
%
before_tcp_pipeline(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?BEFORE_TCP, '$1', '$2'}
  ),
  pipeline(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% after tcp pipeline
%
after_tcp_pipeline(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?AFTER_TCP, '$1', '$2'}
  ),
  pipeline(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% before udp pipeline
%
before_udp_pipeline(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?BEFORE_UDP, '$1', '$2'}
  ),
  pipeline(Filter, Data, Opt).

%%--------------------------------------------------------------------
%
% after udp pipeline
%
after_udp_pipeline(Data, Opt) ->
  Filter = mnesia:dirty_match_object(pipeline,
    {'_', ?AFTER_UDP, '$1', '$2'}
  ),
  pipeline(Filter, Data, Opt).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% pipeline
%
pipeline([], Data, Opt) ->
  {ok, Data, Opt};
pipeline([#pipeline{module=undefined, func=Func}| Tail], Data, Opt) ->
  case apply(Func, [Data, Opt]) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, Opt} ->
      pipeline(Tail, Data, Opt)
  end;
pipeline([#pipeline{module=Module, func=Func}| Tail], Data, Opt) ->
  case apply(Module, Func, [Data, Opt]) of
    {error, Msg} ->
      {error, Msg};
    {ok, Data, Opt} ->
      pipeline(Tail, Data, Opt)
  end.

