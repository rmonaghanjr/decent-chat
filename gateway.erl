-module(gateway).

-include("chat_node.hrl").
-import(chat_node, [loop/2]).

-define(MAX_NODES, 512).
-define(INITIAL_NODES, 1).
-export([start/0]).

start() ->
    Node = spawn(chat_node, loop, [#client_node{last_update=erlang:timestamp(), messages=[]}, []]),
    io:format("started entry node!\n[\n\tmax-nodes=~p\n\tinitial_nodes=~p\n]\n", [?MAX_NODES, ?INITIAL_NODES]),
    Node.

