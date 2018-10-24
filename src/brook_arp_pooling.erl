%%%-------------------------------------------------------------------
%% @doc brook arp pooling
%% @end
%%%-------------------------------------------------------------------

-module(brook_arp_pooling).
-behavior(gen_server).

-record(state, {
  packet,
  interface,
  nexthop,
  count
}).

-type state() ::  #state{}.

-export([start_link/1]).
-export([init/1]).
-export([terminate/2]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).

-export([save_pooling/3]).

%%====================================================================
%% API
%%====================================================================

-spec start_link(integer()) -> {ok, pid()}.
start_link(Timer) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Timer], []).

-spec init([integer()]) -> {ok, []}.
init([Timer]) ->
  spawn(fun() -> pooling(Timer) end),
  {ok, []}.

-spec save_pooling(bitstring(), brook_interface:name(), brook_ip:ip_address()) -> term().
save_pooling(Data, IfName, NextIp) ->
  gen_server:cast(?MODULE, {save_pooling, {Data, IfName, NextIp}}).

handle_info(_Message, Storage) ->
  {noreply, Storage}.

terminate(_Message, _Storage) ->
  ok.

-spec handle_call(term(), term(), term()) -> {reply, term(), term()}.
handle_call({check}, _From, State) ->
  Res = check_pool_packet(State, []),
  {reply, ok, Res};
handle_call(?MODULE, _, State) ->
  {reply, ok, State}.

handle_cast({save_pooling, {Packet, IfName, Nexthop}}, State) ->
  {noreply, [
      #state{packet=Packet, interface=IfName, nexthop=Nexthop, count=0}| State
  ]}.

check_pool_packet([], Res) ->
  Res;
check_pool_packet([
  #state{packet=Packet, interface=IfName,
        nexthop=Nexthop, count=Count}| Tail
], Res) ->
  case brook_arp:get_mac_addr({IfName, Nexthop}) of
    undefined when Count >= 30 ->
      check_pool_packet(Tail, Res);
    undefined ->
      check_pool_packet(
        Tail,
        [#state{packet=Packet, interface=IfName,
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

