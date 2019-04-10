brook
=====

Brook is software router for erlang.

## Goals

Brook provide arp protocol, ip protocol and icmp protocol.
You can implement basic routing and also configure static routing.


## Example

### routing table control
#### add route

add static route

destination ip : {192, 168, 30, 0}

destination subnetmask : {255, 255, 255, 0}

next hop ip : {255, 255, 255, 0}

out interface : "eth1"

```
brook_ip:route(add, static, {192, 168, 30, 0}, {255, 255, 255, 0}, {192,168, 40, 20}, "eth1").
```

### show route

brook route control show atom.
print routing table example.

```
{
  routing_table, % record  name
  s, % add routing table, source route
  {192,168,30,0}, % destination route
  3232243200, % destination route integer
  {255,255,255,0}, % destination route subnet mask
  4294967040, % destination route subnet mask integer
  1, % administrative distance
  0, % metric
  {192,168,40,20}, % next hop ip address
  0, % learng time
  "eth1" % out put interface
}
```

#### show all routing table

```
> brook_ip:route(show).
[{routing_table,s,
                {192,168,30,0},
                3232243200,
                {255,255,255,0},
                4294967040,1,0,
                {192,168,40,20},
                0,"eth1"},
 {routing_table,c,
                {10,0,2,0},
                167772672,
                {255,255,255,0},
                4294967040,0,0,directory_connected,0,"eth0"},
 {routing_table,c,
                {192,168,20,0},
                3232240640,
                {255,255,255,0},
                4294967040,0,0,directory_connected,0,"eth1"}]
```

#### show connect routing table

print direct connect routing table.
put `c` or `connect` as the second argument

```
> brook_ip:route(show, c).
[{routing_table,c,
                {10,0,2,0},
                167772672,
                {255,255,255,0},
                4294967040,0,0,directory_connected,0,"eth0"},
 {routing_table,c,
                {192,168,20,0},
                3232240640,
                {255,255,255,0},
                4294967040,0,0,directory_connected,0,"eth1"}]
```

#### show static routing table

put `s` or `static` as the second argument

```
> brook_ip:route(show, s).
[{routing_table,s,
                {192,168,30,0},
                3232243200,
                {255,255,255,0},
                4294967040,1,0,
                {192,168,40,20},
                0,"eth1"}]
```
