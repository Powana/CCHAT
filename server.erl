-module(server).
-export([start/1,stop/1,messageHandler/1, channel/2] ).

sendMessage(Data, []) -> ok;
sendMessage(Data, [H|T]) ->

    genserver:request(H, Data),
    sendMessage(Data, T).

channel(Name, Users) ->

    receive
        {join, UsrPID} ->
            NewUsers = lists:append(Users, [UsrPID]),

            UserNick = genserver:request(UsrPID, whoami),
            JoinMsg = io_lib:format("~p has joined the fray.", [UserNick]),
            Data = {message_receive, Name, "System", JoinMsg},
            sendMessage(Data, Users),

            channel(Name, NewUsers);

        {leave, Nick} ->
            io:format("Username left: ~n"),
            channel(Name, Users);

        {sndMsg, Msg, User} ->

            UserNick = genserver:request(User, whoami),
            Data = {message_receive, Name, UserNick, Msg},

            sendMessage(Data, Users),
            channel(Name, Users)

    end.

messageHandler(Channels) ->
    receive
        {join, ChannelName, UsrPID} ->
            FoundChannels = length(ets:lookup(Channels, ChannelName)),
            if
            % Channel does not already exist
                FoundChannels == 0 ->
                    io:format("Did not find channel, adding new channel"),
                    Users = [],
                    NewChannelPID = spawn(server, channel, [ChannelName, Users]),
                    ets:insert(Channels, {ChannelName, NewChannelPID}); % Could add users as a list in the tuple.

                true ->
                    io:fwrite("FOUND CHANNEL YAY")
            end,

            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName),
            ChannelPID ! {join, UsrPID},
            messageHandler(Channels);


        {leave, ChannelName, Nick} ->
            ChannelPID = ets:lookup(Channels, ChannelName),
            ChannelPID ! {leave, Nick},
            messageHandler(Channels);


        {message_send, ChannelName, Msg, User} ->
            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName), % todo maybe check for error
            ChannelPID ! {sndMsg, Msg, User},
            messageHandler(Channels)



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
    Channels = ets:new(c, [set, public]),
    SrvPID = spawn(server, messageHandler, [Channels]),
    register(ServerAtom, SrvPID),
    SrvPID.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
