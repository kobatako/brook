-module(packet_receiver).
-behaviour(gen_event).

-include("interface.hrl").

-define(TYPE_ARP,  16#0806).
-define(TYPE_IP, 16#0800).

-export([start_link/1]).
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([terminate/2]).

%%====================================================================
%% API
%%====================================================================

start_link(FD) ->
  gen_event:start_link({local, receiver}),
  gen_event:start({local, receiver}),
  gen_event:add_handler(receiver, ?MODULE, [FD]).

%%--------------------------------------------------------------------

init([FD]) ->
  loop_up(FD),
  {ok, FD}.

%%--------------------------------------------------------------------
loop_up([]) ->
  true;
loop_up([#{ip_fd := FD}| Tail]) ->
  spawn(fun() -> loop(FD) end),
  loop_up(Tail).

%%--------------------------------------------------------------------
% main loop
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
  <<_:48, SourceMacAddr:48, Type:16, Data/bitstring>> = Buf,
  <<S1, S2, S3, S4, S5, S6>> = <<SourceMacAddr:48>>,
  case interface:match({interface, '$1', '$2', '$3', [S1, S2, S3, S4, S5, S6]}) of
    [] ->
      ethernet_type(Type, Data);
    _ ->
      source_my_ip
  end,
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

%%--------------------------------------------------------------------
% IP Protocol
ethernet_type(?TYPE_IP, Data) ->
  ip:packet(Data);

%%--------------------------------------------------------------------
% ARP Protocol
ethernet_type(?TYPE_ARP, Data) ->
  arp:packet(Data);

%%--------------------------------------------------------------------
ethernet_type(_Type, _Data) ->
  false.

