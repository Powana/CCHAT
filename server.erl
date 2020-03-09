-module(server).
-export([start/1,stop/1,messageHandler/1, channel/2] ).

sendMessage(_, _, []) -> ok;
sendMessage(Data, User, [H|T]) ->
    if
      H == User -> % Dont send message to sender
        sendMessage(Data, User, T);
      true ->
        genserver:request(H, Data),
        sendMessage(Data, User, T)
    end.

channel(Name, Users) ->

    receive
        {join, UsrPID} ->
            NewUsers = lists:append(Users, [UsrPID]),

            UserNick = genserver:request(UsrPID, whoami),
            JoinMsg = io_lib:format("~p has joined the fray.", [UserNick]),
            Data = {message_receive, Name, "System", JoinMsg},
            sendMessage(Data, 0, Users),

            channel(Name, NewUsers);

        {leave, UsrPID} ->
            NewUsers = Users -- [UsrPID],

            UserNick = genserver:request(UsrPID, whoami),
            JoinMsg = io_lib:format("~p has left the channel.", [UserNick]),
            Data = {message_receive, Name, "System", JoinMsg},
            sendMessage(Data, 0, Users),

            channel(Name, NewUsers);

        {sndMsg, Msg, User} ->

            UserNick = genserver:request(User, whoami),
            Data = {message_receive, Name, UserNick, Msg},

            sendMessage(Data, User, Users),
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
                    io:fwrite("FOUND CHANNEL")
            end,

            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName),
            ChannelPID ! {join, UsrPID},
            messageHandler(Channels);


        {leave, ChannelName, User} ->
            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName),
            ChannelPID ! {leave, User},
            messageHandler(Channels);


        {message_send, ChannelName, Msg, User} ->
            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName), % todo maybe check for error
            ChannelPID ! {sndMsg, Msg, User},
            messageHandler(Channels)

    end.


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
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
    exit(whereis(ServerAtom)),
    catch(unregister(ServerAtom)),
    ok.
