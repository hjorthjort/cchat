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

handle(State, {disconnect, Pid}) ->
    io:fwrite("in disconnect~n", []),
    case catch(get_user(State, Pid)) of
        undefined ->
            {reply, {error, user_not_connected}, State};
        %User is connected to chat channels
        #user{channels=[_H | _T]} ->
            {reply, {error, leave_channels_first}, State};
        %User is not connected to any channels
        #user{channels=[]} ->
            NewState = State#server_state{ users = [User ||
                    User <- State#server_state.users, User#user.pid /= Pid]},
            io:fwrite("Users: ~p~n", [NewState#server_state.users]),
            {reply, ok, NewState};
        X ->
            io:fwrite("~p~n", [X])
    end;

handle(State, {join, ChannelName, ClientPid}) ->
    Channel = get_channel_atom(State, ChannelName),
    NewState = case lists:member(Channel, State#server_state.channels) of
        false ->
            create_channel(State, Channel);
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

handle(State, {leave, ChannelName, Pid}) ->
    User = get_user(State, Pid),
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

handle(State, {send_message, Channel, Message, Sender}) ->
    User = proplists:get_value(Sender, State#server_state.users),
    io:fwrite("[server] State: ~p~n", [State]),
    case genserver:request(get_channel_atom(State, Channel),
                           {send_message, User, Message}) of 
        {error, user_not_joined} ->
            {reply, {error, user_not_joined}, State};
        ok ->
            {reply, ok, State}
    end.

get_channel_atom(State, Channel) ->
    list_to_atom(State#server_state.name ++ Channel) .

% Returns the user with the given Pid, or `undefined` if user is not connected
get_user( State, Pid) ->
    case [ User || User <- State#server_state.users, User#user.pid == Pid ] of
        [] ->
            undefined;
        [Head] ->
            Head
    end.

create_channel(State, Channel) ->
    genserver:start(Channel, channel:initial_state(Channel), fun channel:handle/2),
    State#server_state{channels = [Channel | State#server_state.channels]}.
