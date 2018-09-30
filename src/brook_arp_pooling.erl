%%%-------------------------------------------------------------------
%% @doc brook arp pooling
%% @end
%%%-------------------------------------------------------------------

-module(brook_arp_pooling).
-behavior(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([terminate/2]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).

-record(arp_pool, {
  packet,
  interface,
  nexthop,
  count
}).

%%====================================================================
%% API
%%====================================================================

start_link(Timer) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Timer], []).

init([Timer]) ->
  spawn(fun() -> pooling(Timer) end),
  {ok, []}.

handle_info(_Message, Storage) ->
  {noreply, Storage}.

terminate(_Message, _Storage) ->
  ok.

handle_call(?MODULE, _, _) ->
  true;
handle_call({check}, _From, State) ->
  Res = check_pool_packet(State, []),
  {reply, ok, Res}.


handle_cast({save_pooling, {Packet, IfName, Nexthop}}, State) ->
  {noreply, [
      #arp_pool{packet=Packet, interface=IfName, nexthop=Nexthop, count=0}| State
  ]}.

check_pool_packet([], Res) ->
  Res;
check_pool_packet([
  #arp_pool{packet=Packet, interface=IfName,
        nexthop=Nexthop, count=Count}| Tail
], Res) ->
  case brook_arp:get_mac_addr({IfName, Nexthop}) of
    undefined when Count >= 30 ->
      check_pool_packet(Tail, Res);
    undefined ->
      check_pool_packet(
        Tail,
        [#arp_pool{packet=Packet, interface=IfName,
        nexthop=Nexthop, count=Count+1}| Res]
      );
    DestMac when is_tuple(DestMac) ->
      brook_sender:send_packet(ip_request, {IfName, tuple_to_list(DestMac), Packet}),
      check_pool_packet(Tail, Res);
    DestMac when is_list(DestMac) ->
      brook_sender:send_packet(ip_request, {IfName, DestMac, Packet}),
      check_pool_packet(Tail, Res)
  end.

pooling(Timer) ->
  gen_server:call(arp_pooling, {check}),
  timer:sleep(Timer),
  pooling(Timer).

