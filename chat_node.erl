-module(chat_node).
-export([loop/2]).

-define(MAX_Nodes, 5).

get_node_count([]) -> 0;
get_node_count(Nodes) -> get_node_count(Nodes, 0).
get_node_count([], Count) -> Count;
get_node_count([_|L], Count) -> get_node_count(L, Count+1).

loop(State, Nodes) ->
    receive
        {create_client, ClientId} -> 
            Count = get_node_count(Nodes),
            case Count of
                ?MAX_Nodes ->
                    NextNode = lists:nth(1, Nodes),
                    NextNode!{create_client, ClientId},
                    loop(State, Nodes);
                _ -> 
                    io:format("added ~p to ~p as a node\n", [ClientId, self()]),
                    loop(State, Nodes ++ [ClientId])
            end;
        {request_count, Sender} ->
            Sender!{node_count, self(), get_node_count(Nodes)},
            loop(State, Nodes);
        {node_count, Pid, Count} ->
            io:format("~p has ~p nodes!\n", [Pid, Count]),
            loop(State, Nodes);
        {get_nodes, Sender} -> 
            Sender!Nodes,
            loop(State, Nodes)
    end.

