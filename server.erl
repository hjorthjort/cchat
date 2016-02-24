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
%%      none
handle(State, {connect, NewPid, NewNick}) ->
    case lists:filter(fun(Elem) -> NewNick == Elem#user.nick end, State#server_state.users) of
        [] ->
            NewState = State#server_state{users = [ #user{ pid=NewPid, nick=NewNick} |
                                                    State#server_state.users ]},
            {reply, ok, NewState};
        [_H | _T] ->
            io:fwrite("~p~n", [_H | _T]),
            {reply, {error, user_already_connected}, State}
    end;

%% Disconnect the user by removing from the server's list of user and returning
%% ok.
%% Parameters:
%%      Pid: The pid of the user wishing to disconnect
%% Possible errors:
%%      none
handle(State, {disconnect, Pid}) ->
    NewState = State#server_state{ users = [User ||
                                            User <- State#server_state.users,
                                            User#user.pid =/= Pid]},
    {reply, ok, NewState};

%% Let user join specified channel on the server they are connected to.
%% If the channel doesn't exist, create it and add it to our list of channels.
%% Tell the channel to connect the user.
%% Parameters:
%%      ChannelName: the name of the channel (starts with '#')
%%      UserPid: The Pid of the client wishing to join
%% Possible errors:
%%      none
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
    genserver:request(Channel, {join, User}),
    {reply, ok, NewState};

%% Let user leave a channel that they are connected to. Assumes channel exists.
%% Parameters:
%%      ChannelName: the name of the channel (starts with '#')
%%      UserPid: the pid of the client wishing to leave the channel
%% Possible errors:
%%      none
handle(State, {leave, ChannelName, UserPid}) ->
    User = get_user(State, UserPid),
    Channel = get_channel_atom(State, ChannelName),
    genserver:request(Channel, {leave, User}),
    {reply, ok, State};

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

%% Creates a new channel and returns a new server state.
%% Parameters:
%%      Atom: the atom to register the new channel process with
%%      UnqualifiedChannelName: channel name without the server prefix
create_channel(State, Atom, UnqualifiedChannelName) ->
    genserver:start(Atom, channel:initial_state(Atom, UnqualifiedChannelName), fun channel:handle/2),
    State#server_state{channels = [Atom | State#server_state.channels]}.
