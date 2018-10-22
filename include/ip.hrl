-record(routing_table, {
  source_route,       % routing learn source
  dest_route,         % destination route
  dest_route_int,     % destination route integer
  subnetmask,         % subnet mask ( number )
  subnetmask_int,     % subnet mask ( number )
  ad,                 % administrative distancec
  metric,             % metric
  nexthop,            % next hop ip
  age,                % destination route learng time
  out_interface       % out put interface
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

