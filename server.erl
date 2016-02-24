-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_state{name = ServerName}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

%% Connect the user by adding user to the server's list of users and returning
%% ok
%% Parameters:
%%      NewPid, NewNick: details for the user
%% Possible errors:
%%      {error, user_already_connected}: when the Pid or Nick is taken.
handle(State, {connect, NewPid, NewNick}) ->
    PossibleCollisons = [ collision || #user{ pid=Pid, nick=Nick } <- State#server_state.users, Pid == NewPid orelse Nick == NewNick],
    case PossibleCollisons of
        % No collissions means we add the user to our list and reply with ok
        [] ->
            NewState = State#server_state{users = [ #user{ pid=NewPid, nick=NewNick} |
                                                    State#server_state.users ]},
            {reply, ok, NewState};
        % If we get a collission we reply with an error and unchanged state
        [_H | _T] ->
            {reply, {error, user_already_connected}, State}
    end;

%% Disconnect the user by removing from the server's list of user and returning
%% ok.
%% Parameters:
%%      Pid: The pid of the user wishing to disconnect
%% Possible errors:
%%      {error, user_not_connected}
%%      {error, leave_channel_first}
%TODO[keep_track]: Should we keep track of the users channels just to handle this? If so, we need to reimplement it : the logic has been removed.
handle(State, {disconnect, Pid}) ->
    io:fwrite("in disconnect~n", []),
    case catch(get_user(State, Pid)) of
       undefined ->
            {reply, {error, user_not_connected}, State};
        %User is connected to chat channels
        %TODO[keep_track]
        #user{channels=[_H | _T]} ->
            {reply, {error, leave_channels_first}, State};
        %User is not connected to any channels
        %TODO[keep_track]
        #user{channels=[]} ->
            NewState = State#server_state{ users = [User ||
                    User <- State#server_state.users, User#user.pid /= Pid]},
            io:fwrite("Users: ~p~n", [NewState#server_state.users]),
            {reply, ok, NewState};
        X ->
            io:fwrite("~p~n", [X])
    end;

%% Let user join specified channel on the server they are connected to.
%% If the channel doesn't exist, create it and add it to our list of channels.
%% Tell the channel to connect the user.
%% Parameters:
%%      ChannelName: the name of the channel (starts with '#')
%%      UserPid: The Pid of the client wishing to join
%% Possible errors:
%%      {error, user_already_joined}
handle(State, {join, ChannelName, UserPid}) ->
    Channel = get_channel_atom(State, ChannelName),
    % It is necessary to create channel even if user fails to connect later,
    % since we need to create a channel to contact it.
    NewState = case lists:member(Channel, State#server_state.channels) of
        false ->
            create_channel(State, Channel, ChannelName);
        true ->
            State
    end,
    User = get_user(State, UserPid),
    case genserver:request(Channel, {join, User}) of
        ok ->
            {reply, ok, NewState};
        {error, user_already_joined} ->
            {reply, {error, user_already_joined}, NewState}
    end;

%% Let user leave a channel that they are connected to.
%% Parameters:
%%      ChannelName: the name of the channel (starts with '#')
%%      UserPid: the pid of the client wishing to leave the channel
%% Possible errors:
%%      {error, user_not_joined} when channel doesn't exist or user is not in channel
handle(State, {leave, ChannelName, UserPid}) ->
    User = get_user(State, UserPid),
    Channel = get_channel_atom(State, ChannelName),
    case lists:member(Channel, State#server_state.channels) of
        true ->
            case genserver:request(Channel, {leave, User}) of
                ok ->
                    {reply, ok, State};
                {error, user_not_joined} ->
                    {reply, {error, user_not_joined}, State}
            end;
        false ->
            {reply, {error, user_not_joined}, State}
    end;

%% Sends a message to a channel.
%% Parameters:
%%      Channel: the name of the channel to send to (starts with '#')
%%      Message: the message to send
%%      SenderPid: the pid to the client wishing to send the message
%% Possible errors:
%%      {error, user_not_joined} when user is not in the specified channel
handle(State, {send_message, Channel, Message, SenderPid}) ->
    User = get_user(State, SenderPid),
    io:fwrite("[server] State: ~p~n", [State]),
    case genserver:request(get_channel_atom(State, Channel),
                           {send_message, User, Message}) of
        {error, user_not_joined} ->
            {reply, {error, user_not_joined}, State};
        ok ->
            {reply, ok, State}
    end.

%% ---------------------------------------------------------------------------

%% Returns the atom used to identify a channel.
%% Parameters:
%%      Channel: the name of the channel (starts with '#')
get_channel_atom(State, Channel) ->
    list_to_atom(State#server_state.name ++ Channel).

%% ---------------------------------------------------------------------------

%% Returns the user with the given Pid, or `undefined` if user is not connected.
%% Parameters:
%%      Pid: the pid to the client associated with the user record we want to get
get_user(State, Pid) ->
    case [ User || User <- State#server_state.users, User#user.pid == Pid ] of
        [] ->
            undefined;
        [Head] ->
            Head
    end.

%% ---------------------------------------------------------------------------

create_channel(State, Atom, UnqualifiedChannelName) ->
    genserver:start(Atom, channel:initial_state(Atom, UnqualifiedChannelName), fun channel:handle/2),
    State#server_state{channels = [Atom | State#server_state.channels]}.
