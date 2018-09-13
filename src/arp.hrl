-record(arp_table, {
  source_ip_addr,
  dest_ip_addr,
  dest_mac_addr,
  type
}).

-record(arpHeader, {
  hardwareType,
  protocol,
  addressLen,
  protocolLen,
  operationCode,
  sourceMacAddress,
  sourceIPAddress,
  destMacAddress,
  destIPAddress
}).


