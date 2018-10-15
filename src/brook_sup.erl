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
        start => {brook_arp_pooling, start_link, [[10]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [brook_arp_pooling]
      }
    ],
    {ok, { {one_for_one, 5, 60}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================

