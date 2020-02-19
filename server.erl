-module(server).
-export([start/1,stop/1,messageHandler/0]).

-record(server_st, {
    channels
}).

initialize_state() ->
    #server_st{
      channels = dict:new()
    }.


channel() ->
    Users = [],

    receive
        {join, Nick} ->
            lists:append(Nick, Users),
            io:format("Username joined: ~n", Nick);
        {leave, Nick} ->
            io:format("Username left: ~n", Nick)
    end.

messageHandler() ->
    io:fwrite("Startdwadawding server"),

    receive
        hellottw ->
            io:fwrite("Hello World uwu");
        {join, ChannelName, Nick} ->
            ChannelExists = dict:is_key(ChannelName, #server_st.channels),
            if
                % Channel does not already exist
                not ChannelExists ->
                    io:fwrite("New Channel"),
                    NewChannelPID = spawn(server, channel, []),
                    NewChannelsDict = dict:store(ChannelName, NewChannelPID, #server_st.channels),

            end,

            ChannelPID = dict:find(ChannelName, ChannelNameDict),
            io:format("PID_INCOMING: "),
            io:format(ChannelPID),
            ChannelPID ! {join, Nick};


        {leave, ChannelName, Nick} ->
            ChannelPID = dict:find(ChannelName, ChannelNameDict),
            ChannelPID ! {leave, Nick}

%%        {message_send, ChannelName, Msg} ->
%%            ;
%%        {nick, NewNick} ->
%%            ;
%%        {leave, whoami} ->
%%            ;
%%        {leave, quit} ->

    end.


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    initialize_state(),
    SrvPID = spawn(server, messageHandler, []),
    register(ServerAtom, SrvPID),
    SrvPID.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
