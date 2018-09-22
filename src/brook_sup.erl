%%%-------------------------------------------------------------------
%% @doc brook top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(brook_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(FD) ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, [FD]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([FD]) ->
    ChildSpecs = [
      #{id => packet_sender,
        start => {packet_sender, start_link, [[FD]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [packet_sender]
      }
    ],
    packet_receiver:start_link(FD),
    {ok, { {one_for_one, 60, 3600}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================

