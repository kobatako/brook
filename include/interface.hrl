-record(interface, {
  name,
  addr,
  netmask,
  hw_addr,
  netaddr
}).

-define(ETH_P_ALL, 16#0300).
-define(SELEF_IP, {127, 0, 0, 1}).
