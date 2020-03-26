-module(server).
-export([start/1,stop/1,messageHandler/1, channel/2] ).


putStrLn(S) ->
    case get(output) of
        off -> ok ;
        _   -> io:fwrite(user, <<"~s~n">>, [S])
    end.

putStrLn(S1, S2) ->
    putStrLn(io_lib:format(S1, S2)).

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
        {join, UsrPID, From} ->
            AlreadyJoined = lists:member(UsrPID, Users),
            if
              AlreadyJoined ->
                From ! {user_already_joined},
                channel(Name, Users);
              true ->
                NewUsers = lists:append(Users, [UsrPID]),
                From ! {ok_join},
                channel(Name, NewUsers)
            end;


        {leave, UsrPID} ->
            NewUsers = Users -- [UsrPID],


            channel(Name, NewUsers);

        {sndMsg, Msg, User, From} ->
            AlreadyJoined = lists:member(User, Users),
            putStrLn("AlreadyJoined = ~p", [AlreadyJoined]),
            UserNick = genserver:request(User, whoami), %FuckYouuuuuuuu

            if
              AlreadyJoined == false ->
                From ! {user_not_joined};
              true ->
                putStrLn("PEEPEE"),
                putStrLn("POOPOO"),

                Data = {message_receive, Name, UserNick, Msg},
                putStrLn("POOPOO"),

                sendMessage(Data, User, Users),
                putStrLn("POOPOO"),
                From ! {ok}
            end,

            channel(Name, Users)

    end.

messageHandler(Channels) ->
    receive
        {join, ChannelName, UsrPID} ->
            FoundChannels = length(ets:lookup(Channels, ChannelName)),
            if
            % Channel does not already exist
                FoundChannels == 0 ->
                    Users = [],
                    NewChannelPID = spawn(server, channel, [ChannelName, Users]),
                    ets:insert(Channels, {ChannelName, NewChannelPID}); % Could add users as a list in the tuple.

                true ->
                    pass
            end,

            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName),
            ChannelPID ! {join, UsrPID, self()},
            receive
              {ok_join} ->
                UsrPID ! {ok};
              {user_already_joined} ->
                UsrPID ! {user_already_joined}
            end,
            messageHandler(Channels);


        {leave, ChannelName, User} ->
            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName),
            ChannelPID ! {leave, User},
            messageHandler(Channels);


        {message_send, ChannelName, Msg, User} ->
            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName), % todo maybe check for error
            ChannelPID ! {sndMsg, Msg, User, self()},
            putStrLn("SFJESIOFJIOJ"),
            receive
              {ok} ->
                putStrLn("OOOKK"),
                User ! {ok};
              {user_not_joined} ->
                putStrLn("USERNOTJOIIINED"),
                User ! {user_not_joined}
            end,
            putStrLn("HELLOFUCKFACE"),
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
    %io_lib:format("~p is ServerAtom, ~p is SrvPid.", [ServerAtom, SrvPID]),
    catch(unregister(ServerAtom)),
    register(ServerAtom, SrvPID),
    SrvPID.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    exit(whereis(ServerAtom), ok),
    catch(unregister(ServerAtom)),
    ok.
