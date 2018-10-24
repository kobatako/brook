-record(arp_table, {
  source_ip_addr,
  dest_ip_addr,
  dest_mac_addr,
  type
}).

-record(arp_header, {
  hw_type,
  protocol,
  address_len,
  protocol_len,
  operation_code,
  source_mac_addr,
  source_ip_addr,
  dest_mac_addr,
  dest_ip_addr
}).

