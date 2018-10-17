%%%-------------------------------------------------------------------
%% @doc brook top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(brook_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecs = [
      #{id => brook_capture_sup,
        start => {brook_capture_sup, start_link, [[]]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [brook_capture_sup]
      }
    ],
    {ok, { {one_for_one, 5, 60}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================

