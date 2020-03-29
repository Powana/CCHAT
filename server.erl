-module(server).
-export([start/1,stop/1,messageHandler/2, channel/2, sendMessage/3] ).


sendMessage(_, _, []) -> exit(self());
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


        {leave, UsrPID, From} ->
            AlreadyJoined = lists:member(UsrPID, Users),
            if
              AlreadyJoined == false ->
                From ! {user_not_joined},
                channel(Name, Users);
              true ->
                NewUsers = Users -- [UsrPID],
                From ! {ok},
                channel(Name, NewUsers)
            end;


        {sndMsg, Msg, User, Nick, From} ->
            AlreadyJoined = lists:member(User, Users),

            if
              AlreadyJoined == false ->
                From ! {user_not_joined};
              true ->
                Data = {message_receive, Name, Nick, Msg},

                spawn(server, sendMessage, [Data, User, Users]),
                From ! {ok}
            end,
            channel(Name, Users)

    end.

messageHandler(Channels, Nicks) ->
    receive
        {join, ChannelName, Nick, UsrPID} ->
            % New user with new nick joining check
            NickAdded = lists:member(Nick, Nicks),
            if
              NickAdded ->
                NewNicks = Nicks;
              true ->
                NewNicks = lists:append(Nicks, [Nick])
            end,

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
            messageHandler(Channels, NewNicks);

        {leave, ChannelName, User} ->
            LookupResult = ets:lookup(Channels, ChannelName),
            if
              LookupResult == [] -> % Channel does not exist
                User ! {server_not_reached};
              true ->
                [{_, ChannelPID}] = LookupResult,
                ChannelPID ! {leave, User, self()},
                receive
                  {ok} ->
                    User ! {ok};
                  {user_not_joined} ->
                    User ! {user_not_joined}
                end
            end,
            messageHandler(Channels, Nicks);

        {message_send, ChannelName, Msg, Nick, User} ->
            LookupResult = ets:lookup(Channels, ChannelName),
            if
              LookupResult == [] -> % Channel does not exist
                User ! {server_not_reached};
              true ->
                [{_, ChannelPID}] = LookupResult,
                ChannelPID ! {sndMsg, Msg, User, Nick, self()},
                receive
                  {ok} ->
                    User ! {ok};
                  {user_not_joined} ->
                    User ! {user_not_joined}
                end
            end,
            messageHandler(Channels, Nicks);

        {check_nick, Nick, OldNick, From} ->
            NickTaken = lists:member(Nick, Nicks),
            if
              NickTaken ->
                From ! {nick_taken},
                messageHandler(Channels, Nicks);
              true ->
                NewNicksTemp = lists:append(Nicks, [Nick]),
                NewNicks = NewNicksTemp -- [OldNick],
                From ! {ok},
                messageHandler(Channels, NewNicks)
            end

    end.



% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Channels = ets:new(c, [set, public]),
    SrvPID = spawn(server, messageHandler, [Channels, []]),
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
