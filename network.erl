-module(network).
-import(net_peer, [new_peer/1]).

-export([build/0]).

build() ->
    P1 = net_peer:new_peer("peer1", true),
    P2 = net_peer:new_peer("peer2", false),
    P3 = net_peer:new_peer("peer3", false),
    P4 = net_peer:new_peer("peer4", false),
    P5 = net_peer:new_peer("peer5", false),
    P1!{add_node, P2},
    P2!{add_node, P3},
    P3!{add_node, P4},
    P4!{add_node, P5},
    P1.
