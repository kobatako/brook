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
      #{id => main,
        start => {main, start_link, [FD]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [main]
      },
      #{id => packet_sender,
        start => {packet_sender, start_link, [[]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [packet_sender]
      }
    ],
    % supervisor:start_child({gloabl, main}, ChildSpecs),
    {ok, { {one_for_one, 60, 3600}, ChildSpecs} }.
    % {ok, { {one_for_one, 60, 3600}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

