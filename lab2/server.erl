-module(server).
-export([start/1,stop/1]).
-import(string, [equal/2]).

-record(server_st, {
    channels,
    nicks
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, #server_st{channels = [], nicks = []}, fun handler/2). % start a process for main server with an array for channels


    
handler(St, {join, Channel, Client}) ->
    case lists:member(Channel, St#server_st.channels) of % check if channel already in main server state, i.e channels list
        true -> Result = genserver:request(list_to_atom(Channel), {join, Client}), % if true get reply from channel process
            case Result of
                joined -> {reply, joined, St}; % if joined now reply to client with joined atom, dont change state
                in_channel -> {reply, in_channel, St} % if joined already reply to client with in_channel atom, dont change state
            end;
        % If channel not in main server state. Create new channel process with the joining client in clients list, i.e channel state
        false -> genserver:start(list_to_atom(Channel), [Client], fun channel/2),
            {reply, joined, St#server_st{channels = [Channel | St#server_st.channels]}}
    end;

handler(St, stop_channels) ->
    lists:foreach(
        fun(Channel) ->
            genserver:stop(list_to_atom(Channel))
        end, St#server_st.channels),
    {reply, ok, []};

handler(St, {add_nick, Nick}) ->
    case lists:member(Nick, St#server_st.nicks) of
        false -> {reply, ok, St#server_st{nicks = [Nick | St#server_st.nicks]}};
        true -> {reply, ok, St}
    end;

handler(St, {check_nick, Nick, NewNick}) ->
    case lists:member(NewNick, St#server_st.nicks) of
        true -> {reply, taken, St};
        false -> {reply, free, St#server_st{nicks = replace(Nick, NewNick, St#server_st.nicks)}}
    end.
    

channel(Clients, {join, Client}) ->
    case lists:member(Client, Clients) of % Check if client is in channel state, i.e list of clients for a channel
        true -> {reply, in_channel, Clients}; % if true reply with in_channel, because we are already joined
        false -> {reply, joined, [Client | Clients]} % if false reply with joined to tell the client of a successful join
    end;

channel(Clients, {leave, Client}) ->
    case lists:member(Client, Clients) of % Check if client is in channel state, i.e list of clients for a channel
        false -> {reply, not_in_channel, Clients}; % if true reply with in_channel, because we are already joined
        true -> {reply, left, lists:delete(Client, Clients)} % if false reply with joined to tell the client of a successful join
    end;

channel(Clients, {message_send, Channel, Msg, Nick, Client}) ->
    case lists:member(Client, Clients) of % Check if client is in channel state, i.e list of clients for a channel
        false -> {reply, failed, Clients}; 
        true -> 
            spawn(fun() -> lists:foreach(
            fun (Pid) ->
                if 
                    Pid == Client -> skip;
                    true -> genserver:request(Pid, {message_receive, Channel, Nick, Msg})
                end
            end, Clients) end),
            {reply, delivered, Clients} 
    end;

channel(Clients, {check_nick, Nick, From}) ->
    Nicks = lists:map(fun (Client) ->
            case Client == From of
                false -> genserver:request(Client, whoami);
                true -> ""
            end
        end,
        Clients
    ),
    case lists:member(Nick, Nicks) of 
        true -> {reply, taken, Clients};
        false -> {reply, free, Clients}
    end.
    

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).

replace(Old, New, List) ->
    NewList = lists:delete(Old, List),
    [New | NewList].
