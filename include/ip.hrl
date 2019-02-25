-record(routing_table, {
  source_route :: atom(),       % routing learn source
  dest_route :: tuple(),        % destination route
  dest_route_int :: integer(),  % destination route integer
  subnetmask :: tuple(),        % subnet mask ( number )
  subnetmask_int :: integer(),  % subnet mask ( number )
  ad :: integer(),              % administrative distance
  metric :: integer(),          % metric
  nexthop :: tuple() | atom(),  % next hop ip
  age :: integer(),             % destination route learng time
  out_interface :: string()     % out put interface
}).

% source route type
% direct connect route
-define(SOURCE_DIRECT, c).
% static route
-define(SOURCE_STATIC, s).

-define(NEXTHOP_DIRECT, directory_connected).

-define(DEFAULT_ROUTE, 255).
% len IP address
-define(IP_LEN, 32).

-define(ICMP_PROTOCOL, 1).
-define(TCP_PROTOCOL, 6).
-define(UDP_PROTOCOL, 17).

