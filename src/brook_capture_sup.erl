%%%-------------------------------------------------------------------
%% @doc brook packet capture supiervisor
%% @end
%%%-------------------------------------------------------------------

-module(brook_capture_sup).

-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% start link
%
start_link([]) ->
  mnesia:start(),
  Listen = brook_interface:init(),
  brook_arp:init(),
  brook_ip:init(),
  brook_pipeline:init(),
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Listen]).

%%--------------------------------------------------------------------
%
% init
%
init([FD]) ->
    ChildSpecs = [
      #{id => brook_sender,
        start => {brook_sender, start_link, [[FD]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [brook_sender]
      },
      #{id => brook_receiver,
        start => {brook_receiver, start_link, [FD]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [brook_receiver]
      },
      #{id => brook_arp_pooling,
        start => {brook_arp_pooling, start_link, [10]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [brook_arp_pooling]
      }
    ],
    {ok, { {one_for_one, 5, 60}, ChildSpecs} }.
