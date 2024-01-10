-module(net_peer).

-include("net_peer.hrl").

-export([main_loop/1, ack_loop/1, join_loop/1, new_peer/2]).

%-record(peer_state, {info, messages, next_peer, prev_peer}).
%-record(peer_info, {name, joined_at, last_update, activity_index}).
%-record(chat_message, {time_sent, sender, contents}).

new_peer(Name, IsOrigin) ->
    Info = #peer_info{name=Name,
                      joined_at=erlang:timestamp(),
                      last_update=0, 
                      activity_index=0},
    Node = spawn(net_peer, join_loop, [Info]),
    case IsOrigin of
        true ->
            Node!{accept, #peer_state{info=Info, messages=[], next_peer=Node, prev_peer=Node}},
            Node;
        false ->
            Node
    end.

join_loop(Info) ->
    receive
        {accept, State} ->
            io:format("[accept, ~s] ~p joined!~n", [Info#peer_info.name, self()]),
            self()!{callout},
            main_loop(State);
        {request_compliance, Sender} ->
            Sender!{candidate_complied, self(), Info},
            join_loop(Info);
        {reject, Reason} ->
            io:format("[reject, ~s] ~p was rejected for '~s'~n", [Info#peer_info.name, self(), Reason])
    end.

ack_loop(PeerState) ->
    receive
        {ack, _} ->
            main_loop(PeerState);
        {done, _} ->
            PeerState#peer_state.prev_peer!{ack, {print, self()}},
            main_loop(PeerState)
    end.

main_loop(PeerState) ->
    receive
        {add_node, Pid} ->
            Pid!{request_compliance, self()},
            main_loop(PeerState);
        {set_next, Pid} ->
            NewState = #peer_state{
                          info=PeerState#peer_state.info,
                          messages=PeerState#peer_state.messages,
                          prev_peer=PeerState#peer_state.prev_peer,
                          next_peer=Pid
                         },
            main_loop(NewState);
        {set_prev, Pid} ->
            NewState = #peer_state{
                          info=PeerState#peer_state.info,
                          messages=PeerState#peer_state.messages,
                          prev_peer=Pid,
                          next_peer=PeerState#peer_state.next_peer
                         },
            main_loop(NewState);
        {candidate_complied, Pid, Info} ->
            io:format("~p complied!~n", [Pid]),
            PidState = #peer_state{
                        info=Info, 
                        messages=[],
                        next_peer=self(),
                        prev_peer=PeerState#peer_state.prev_peer},
            PidState#peer_state.prev_peer!{set_next, Pid},
            UpdatedState = #peer_state{
                          info=PeerState#peer_state.info,
                          messages=PeerState#peer_state.messages,
                          next_peer=PeerState#peer_state.next_peer,
                          prev_peer=Pid},
            PidState#peer_state.next_peer!{set_prev, Pid},
            Pid!{accept, PidState},
            main_loop(UpdatedState);
        {callout} ->
            io:format("[callout, ~s] p:~p<-(o:~p)->n:~p~n", [
                                                                      PeerState#peer_state.info#peer_info.name,
                                                                      PeerState#peer_state.prev_peer,
                                                                      self(),
                                                                      PeerState#peer_state.next_peer]),
            main_loop(PeerState);
        {print, Origin} ->
            io:format("[print, ~s] p:~p<-(o:~p)->n:~p~n", [
                                                                      PeerState#peer_state.info#peer_info.name,
                                                                      PeerState#peer_state.prev_peer,
                                                                      self(),
                                                                      PeerState#peer_state.next_peer]),
            if
                PeerState#peer_state.next_peer == Origin ->
                    Origin!{done, print},
                    PeerState#peer_state.prev_peer!{ack, {print, Origin}},
                    ack_loop(PeerState);
                self() == Origin ->
                    PeerState#peer_state.next_peer!{print, Origin},
                    ack_loop(PeerState);
                PeerState#peer_state.prev_peer == Origin ->
                    PeerState#peer_state.next_peer!{print, Origin},
                    ack_loop(PeerState);
                true ->
                    PeerState#peer_state.next_peer!{print, Origin},
                    PeerState#peer_state.prev_peer!{ack, {print, Origin}},
                    ack_loop(PeerState)
            end
    end.
