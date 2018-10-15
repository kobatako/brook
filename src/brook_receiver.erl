-module(brook_receiver).
-behaviour(gen_event).

-include("interface.hrl").

-export([start_link/1]).
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([terminate/2]).

%%====================================================================
%% API
%%====================================================================

start_link(FD) ->
  Res = gen_event:start_link({local, receiver}), gen_event:start({local, receiver}),
  gen_event:add_handler(receiver, ?MODULE, [FD]),
  Res.

%%--------------------------------------------------------------------

init([FD| _]) ->
  loop_up(FD),
  {ok, FD}.

%%--------------------------------------------------------------------
loop_up([]) ->
  true;
loop_up([#{ip_fd := FD}| Tail]) ->
  spawn_link(fun() -> loop(FD) end),
  loop_up(Tail).

%%--------------------------------------------------------------------
%
% main loop
%
loop(FD) ->
  case procket:recv(FD, 8192) of
    {error, eagain} ->
      false;
    {error, _} ->
      false;
    {ok, Buf} ->
      gen_event:notify(receiver, Buf),
      true;
    _ ->
      true
  end,
  loop(FD).

%%--------------------------------------------------------------------
% handle event
handle_event(Buf, Fd) ->
  brook_ethernet:receive_packet(Buf),
  {ok, Fd}.

%%--------------------------------------------------------------------
handle_call(Buf, Fd) ->
  {ok, Buf, Fd}.

%%--------------------------------------------------------------------
% terminate evnet
terminate(_Arg, _State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

