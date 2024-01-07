-module(net_peer).

-include("net_peer.hrl").

-export([mainLoop/1, ackLoop/1, joinLoop/1, newPeer/2]).

%-record(peer_state, {info, messages, next_peer, prev_peer}).
%-record(peer_info, {name, joined_at, last_update, activity_index}).
%-record(chat_message, {time_sent, sender, contents}).

newPeer(Name, IsOrigin) ->
    Info = #peer_info{name=Name,
                      joined_at=erlang:timestamp(),
                      last_update=0, 
                      activity_index=0},
    Node = spawn(net_peer, joinLoop, [Info]),
    case IsOrigin of
        true ->
            Node!{accept, #peer_state{info=Info, messages=[], next_peer=Node, prev_peer=Node}},
            Node;
        false ->
            Node
    end.

joinLoop(Info) ->
    receive
        {accept, State} ->
            io:format("~p joined!", [self()]),
            self()!{print, self()},
            mainLoop(State);
        {request_compliance, Sender} ->
            Sender!{candidate_complied, self(), Info},
            joinLoop(Info);
        {reject, Reason} ->
            io:format("~p was rejected for '~s'~n", [self(), Reason])
    end.

ackLoop(PeerState) ->
    receive
        {ack, _} ->
            mainLoop(PeerState);
        {done, Message} ->
            io:format("reached end of ring with message {~s}~n!", [atom_to_list(Message)]),
            mainLoop(PeerState)
    after 3000 ->
        io:format("Error occured while waiting on ~p~n", [PeerState#peer_state.next_peer]),
        mainLoop(PeerState)
    end.

mainLoop(PeerState) ->
    receive
        {add_node, Pid} ->
            Pid!{request_compliance, self()},
            mainLoop(PeerState);
        {candidate_complied, Pid, Info} ->
            io:format("complied~n!"),
            PState = #peer_state{
                        info=Info, 
                        messages=[],
                        next_peer=self(),
                        prev_peer=PeerState#peer_state.prev_peer},
            NewState = #peer_state{
                          info=PeerState#peer_state.info,
                          messages=PeerState#peer_state.messages,
                          next_peer=PeerState#peer_state.next_peer,
                          prev_peer=Pid},
            Pid!{accept, PState},
            mainLoop(NewState);
        {print, Origin} ->
            PeerState#peer_state.prev_peer!{ack, {print, Origin}},
            io:format("I am [~s<~p>] with connections p:~p,n:~p!~n", [
                                                                      PeerState#peer_state.info#peer_info.name,
                                                                      self(),
                                                                      PeerState#peer_state.prev_peer,
                                                                      PeerState#peer_state.next_peer]),
            if
                PeerState#peer_state.next_peer == Origin ->
                    self()!{done, print},
                    ackLoop(PeerState);
                true ->
                    ackLoop(PeerState)
            end
    end.
