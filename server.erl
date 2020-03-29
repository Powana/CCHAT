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


        {leave, UsrPID} ->
            AlreadyJoined = lists:member(UsrPID, Users),
            if
              AlreadyJoined == false ->
                UsrPID ! {user_not_joined},
                channel(Name, Users);
              true ->
                NewUsers = Users -- [UsrPID],
                UsrPID ! {ok},
                channel(Name, NewUsers)
            end;


        {sndMsg, Msg, User, Nick} ->
            AlreadyJoined = lists:member(User, Users),

            if
              AlreadyJoined == false ->
                User ! {user_not_joined};
              true ->
                Data = {message_receive, Name, Nick, Msg},
                spawn(server, sendMessage, [Data, User, Users]), % Each message gets it's own process to ensure no blockage
                User ! {ok}
            end,
            channel(Name, Users)

    end.

messageHandler(Channels, Nicks) ->
    receive
        {join, ChannelName, Nick, UsrPID} ->
            NickAdded = lists:member(Nick, Nicks),
            if
              NickAdded ->
                NewNicks = Nicks;
              true -> % New user with generated nick added to list of current nicks
                NewNicks = lists:append(Nicks, [Nick])
            end,

            FoundChannels = length(ets:lookup(Channels, ChannelName)),
            if
            % Channel does not already exist
                FoundChannels == 0 ->
                    Users = [],
                    NewChannelPID = spawn(server, channel, [ChannelName, Users]),
                    ets:insert(Channels, {ChannelName, NewChannelPID});

                true ->
                    pass
            end,

            [{_, ChannelPID}] = ets:lookup(Channels, ChannelName),
            ChannelPID ! {join, UsrPID, self()},
            receive
              {ok_join} ->
                UsrPID ! {ok, Channels};
              {user_already_joined} ->
                UsrPID ! {user_already_joined}
            end,
            messageHandler(Channels, NewNicks);

        {check_nick, Nick, OldNick, From} ->
            NickTaken = lists:member(Nick, Nicks),
            if
              NickTaken ->
                From ! {nick_taken},
                messageHandler(Channels, Nicks);
              true -> % Nick is not taken, update list of current nicks
                NewNicksTemp = lists:append(Nicks, [Nick]),
                NewNicks = NewNicksTemp -- [OldNick],
                From ! {ok},
                messageHandler(Channels, NewNicks)
            end;

        {get_channels, From} -> % Simple getter for the Channels ets
          From ! {Channels},
          messageHandler(Channels, Nicks);

        {stop_channels, From} ->
          lists:foreach(fun stop_channel/1, ets:tab2list(Channels)),
          From ! {ok},
          messageHandler(Channels, Nicks)
    end.

stop_channel(ChannelTuple) ->
  {_, PID} = ChannelTuple,
  exit(PID, kill).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Channels = ets:new(c, [set, public]), % A lookup table between Names and PIDs
    SrvPID = spawn(server, messageHandler, [Channels, []]),
    catch(unregister(ServerAtom)),
    register(ServerAtom, SrvPID),
    SrvPID.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    ServerAtom ! {stop_channels, self()},
    receive
      {ok} -> % Wait for all channels to be stopped before stopping server
        exit(whereis(ServerAtom), ok),
        catch(unregister(ServerAtom))
    end,
    ok.
