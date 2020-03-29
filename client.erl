-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = null
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    {St#client_st.server, node()} ! {join, Channel, St#client_st.nick, self()},
    receive
      {ok, Channels} ->
        {reply, ok, St#client_st{channels=Channels}};
      {user_already_joined} ->
        {reply, {error,user_already_joined,"User already joined"}, St}

    after
      3000 ->
        {reply, {error,server_not_reached,"Server not reached"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
  if
    St#client_st.channels == null ->
        {reply, {error,user_not_joined,"User not joined channel"}, St};
      true ->
        LookupResult = ets:lookup(St#client_st.channels, Channel),
        if
          LookupResult == [] ->
            {reply, {error,user_not_joined,"User not joined channel"}, St};
          true ->
            [{_, LeaveChannel}] = LookupResult,
            LeaveChannel ! {leave, self()},
            %{St#client_st.server, node()} ! {leave, Channel, self()},

            receive
              {ok} ->
                {reply, ok, St};
              {user_not_joined} ->
                {reply, {error,user_not_joined,"User not joined channel"}, St}
            after
              3000 ->
                {reply, {error,server_not_reached,"Server not reached"}, St}
            end
        end
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
  {St#client_st.server, node()} ! {get_channels, self()},
  receive
    {C} ->
      Channels = C
  after
    900 ->
      Channels = St#client_st.channels
  end,

  LookupResult = ets:lookup(Channels, Channel),
  if
    LookupResult == [] ->
      {reply,{error,server_not_reached,"Server not reached"} , St#client_st{channels=Channels}};
    true ->
      [{_, MsgChannel}] = LookupResult,
      Nick = St#client_st.nick,
      MsgChannel ! {sndMsg, Msg, self(), Nick},
      receive
        {ok} ->
          {reply, ok, St} ;
        {user_not_joined} ->
          {reply, {error,user_not_joined,"User not joined channel"}, St#client_st{channels=Channels}}
      after
        3000 ->
          {reply,{error,server_not_reached,"Server not reached"} , St#client_st{channels=Channels}}
      end
  end;


% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {St#client_st.server, node()} ! {check_nick, NewNick, St#client_st.nick, self()},
    receive
      {ok} ->
        {reply, ok, St#client_st{nick = NewNick}} ;
      {nick_taken} ->
        {reply, {error, nick_taken, "Nick taken"}, St}
    end;


% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    io:fwrite(Msg),
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
