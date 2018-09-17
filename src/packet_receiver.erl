-module(packet_receiver).
-behaviour(gen_event).

-include("interface.hrl").

-define(ETH_P_ALL, 16#0300).

-define(ARP_TYPE,  16#0806).
-define(ICMP_TYPE, 16#0800).

-export([start_link/1]).
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([terminate/2]).

start_link(FD) ->
  gen_event:start_link({local, receiver}),
  gen_event:start({local, receiver}),
  gen_event:add_handler(receiver, ?MODULE, [FD]).

init([FD]) ->
  spawn(fun() -> loop(FD) end),
  {ok, FD}.

% main loop
loop(FD) ->
  {Result, Buf} = procket:recvfrom(FD, 4096),
  case Result of
    error ->
      false;
    ok ->
      gen_event:notify(receiver, Buf),
      true;
    _ ->
      true
  end,
  loop(FD).

% handle event
handle_event(Buf, Fd) ->
  <<_:48, SourceMacAddr:48, Type:16, Data/bitstring>> = Buf,
  <<S1, S2, S3, S4, S5, S6>> = <<SourceMacAddr:48>>,
  case ets:match(interface, {interface, '$1', '$2', '$3', [S1, S2, S3, S4, S5, S6]}) of
    [] ->
      ethernet_type(Type, Data);
    _ ->
      source_my_ip
  end,
  {ok, Fd}.

handle_call(Buf, Fd) ->
  {ok, Buf, Fd}.

% terminate evnet
terminate(_Arg, _State) ->
  ok.

% ICMP Protocol
ethernet_type(?ICMP_TYPE, Data) ->
  icmp:packet(Data);

% ARP Protocol
ethernet_type(?ARP_TYPE, Data) ->
  arp:packet(Data);
ethernet_type(_, _) ->
  false.

