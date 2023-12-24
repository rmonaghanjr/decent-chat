-module(chat_node).
-include("chat_node.hrl").
-export([loop/2]).

-define(MAX_NODES, 5).

get_node_count([]) -> 0;
get_node_count(Nodes) -> get_node_count(Nodes, 0).
get_node_count([], Count) -> Count;
get_node_count([_|L], Count) -> get_node_count(L, Count+1).

broadcast_message([], _) -> ok;
broadcast_message([H|T], Message) ->
    H!Message,
    broadcast_message(T, Message).

election_wait_loop(State, Nodes, RecvCount, CurrCandidate) ->
    receive
        {found_candidate, Origin, NodePid, ParentPid} ->
            Count = get_node_count(Nodes),
            if
                self() == Origin ->
                    io:format("reached req origin ~p!\n", [self()]),
                    CurrCandidate!{add_node, NodePid},
                    NodePid!{found_parent, ParentPid},
                    loop(State, Nodes);
                Count == 0 ->
                    io:format("using candidate ~p\n", [ParentPid]),
                    State#client_node.parent!{found_candidate, Origin, NodePid, ParentPid},
                    loop(State, Nodes);
                Count == RecvCount + 1 ->
                    io:format("using candidate ~p\n", [CurrCandidate]),
                    State#client_node.parent!{found_candidate, Origin, NodePid, CurrCandidate},
                    loop(State, Nodes);
                RecvCount == 0 ->
                    election_wait_loop(State, Nodes, RecvCount + 1, ParentPid);
                true ->
                    election_wait_loop(State, Nodes, RecvCount + 1, CurrCandidate)
            end
    end.

loop(State, Nodes) ->
    receive
        {begin_election, Sender, NodePid} ->
            Count = get_node_count(Nodes),
            if
                Count < 5 ->
                    io:format("looking at candidate ~p\n", [self()]),
                    if 
                        State#client_node.parent == origin ->
                            self()!{add_node, NodePid},
                            NodePid!{found_parent, self()},
                            loop(State, Nodes);
                        true -> 
                            State#client_node.parent!{found_candidate, Sender, NodePid, self()},
                            loop(State, Nodes)
                    end;
                true ->
                    broadcast_message(Nodes, {begin_election, Sender, NodePid}),
                    election_wait_loop(State, Nodes, 0, self())
            end;
        {add_node, Pid} ->
            loop(State, Nodes ++ [Pid]);
        {found_parent, Pid} ->
            NState = #client_node{
                        parent=Pid,
                        last_update=erlang:timestamp(),
                        messages=[]},
            io:format("~p has new node ~p!\n", [Pid, self()]),
            loop(NState, [])
    end.

