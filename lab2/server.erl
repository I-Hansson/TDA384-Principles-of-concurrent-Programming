-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, [], fun handler/2). % start a process for main server with an array for channels

    
handler(St, {join, Channel, Client}) ->
    case lists:member(Channel, St) of % check if channel already in main server state, i.e channels list
        true -> Result = genserver:request(list_to_atom(Channel), {join, Client}), % if true get reply from channel process
            case Result of
                joined -> {reply, joined, St}; % if joined now reply to client with joined atom, dont change state
                in_channel -> {reply, in_channel, St} % if joined already reply to client with in_channel atom, dont change state
            end;
        % If channel not in main server state. Create new channel process with the joining client in clients list, i.e channel state
        false -> genserver:start(list_to_atom(Channel), [Client], fun channel/2),
            {reply, joined, [Channel | St]}
    end.


channel(Clients, {join, Client}) ->
    case lists:member(Client, Clients) of % Check if client is in channel state, i.e list of clients for a channel
        true -> {reply, in_channel, Clients}; % if true reply with in_channel, because we are already joined
        false -> {reply, joined, [Client | Clients]} % if false reply with joined to tell the client of a successful join
    end;

channel(Clients, {leave, Client}) ->
    case lists:member(Client, Clients) of % Check if client is in channel state, i.e list of clients for a channel
        false -> {reply, in_channel, Clients}; % if true reply with in_channel, because we are already joined
        true -> {reply, left, lists:delete(Client, Clients)} % if false reply with joined to tell the client of a successful join
    end;

channel(Clients, {message_send, Client, Msg, Nick, Channel}) ->
    case lists:member(Client, Clients) of % Check if client is in channel state, i.e list of clients for a channel
        false -> {reply, failed, Clients}; % if true reply with in_channel, because we are already joined
        true -> 
            spawn(fun() -> lists:foreach(
            fun (Pid) ->
                if 
                    Pid == Client -> skip;
                    true -> genserver:request(Pid, {message_receive, Channel, Nick, Msg})
                end
            end, Clients) end),
            {reply, delivered, Clients} % if false reply with joined to tell the client of a successful join
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
