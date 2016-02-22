-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_state{}.

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
            CurrentUsers = State#server_state.users,
            NewState = State#server_state{ users = [User || 
                    User <- State#server_state.users, User#user.pid /= Pid]},
            io:fwrite("Users: ~p~n", [NewState#server_state.users]),
            {reply, ok, NewState};
        X ->
            io:fwrite("~p~n", [X])
    end;

handle(State, {join, ChannelName, ClientPid}) ->
    % Does channel exist?
    case get_channel(State, ChannelName) of
        % No, create channel
        undefined ->
            NewState = create_channel(State, ChannelName, ClientPid),
            {reply, ok, NewState};
        Channel ->
            % Is user in channel?
            case is_user_in_channel(Channel, ClientPid) of
                % No, add user to the channel
                false ->
                    NewState = add_user_to_channel(State, Channel, ClientPid),
                    {reply, ok, NewState};
                % Yes, throw error
                true ->
                    {reply, {error, user_already_joined}, State}
            end
    end.

handle(State, {leave, Channel, Pid}) ->
    {Nick, CurrentChannels} = get_user(State, Pid),
    case lists:member(Channel, CurrentChannels) of
        true ->
            TmpList = proplists:delete(Pid, State#server_state.users),
            NewUsers = [ {Pid, {Nick, lists:delete(Channel, CurrentChannels)}} | TmpList],
            NewState = State#server_state{ users = NewUsers },
            io:fwrite("Users: ~p~n", [NewState#server_state.users]),
            {reply, ok, NewState};
        false ->
            {reply, {error, user_not_joined}, State}
    end;

handle(State, {send_message, Channel, Message, Sender}) ->
    {Nick, ConnectedChannels} = proplists:get_value(Sender, State#server_state.users),
    case lists:member(Channel, ConnectedChannels) of
        false ->
            {reply, {error, user_not_joined}, State};
        true ->
            [ genserver:request(Pid, {incoming_msg, Channel, Nick, Message }) ||
                {Pid, {_, Channels}} <- State#server_state.users, lists:member(Channel, Channels),
                Pid /= Sender],
            {reply, ok, State}
    end;
        
%TODO: Remove this
handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi! You failed at pattern matching!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.

% Returns the user with the given Pid, or `undefined` if user is not connected
get_user( State, Pid) ->
    case [ User || User <- State#server_state.users, User#user.pid == Pid ] of
        [] ->
            undefined;
        [Head] ->
            Head;
        List ->
            List
    end.

% Updates a single user and returns the new server state
update_user(State, User) ->
    {Pid, _} = User,
    State#server_state{ users = [User | proplists:delete(Pid, State#server_state.users)]}.

% Returns the channel with the given name, or `undefined` if the channel doesn't exist
get_channel(State, ChannelName) ->
    case lists:filter(fun(Channel) -> Channel#channel.name =:= ChannelName, State#server_state.channels) of
        % No matches, channel doesn't exist
        [] ->
            undefined;
        % One match, return channel
        [H] ->
            H
    end.

is_user_in_channel(Channel, ClientPid) ->
    case lists:filter(fun(User) -> User#user.pid =:= ClientPid, Channel#channel.users) of
        % No matches, user is not in channel
        [] ->
            false;
        % One match, user is in channel
        [_H] ->
            true
    end

add_user_to_channel(State, Channel, ClientPid) ->
    FilteredChannels = lists:filter(fun(C) -> C /= Channel, State#server_state.channels),
    NewChannel = Channel#channel{users = [ClientPid | Channel#channel.users]}},
    State#server_state{channels = [NewChannel | FilteredChannels]}.

create_channel(State, ChannelName, ClientPid) ->
    ChannelPid = spawn(fun channel_loop/0),
    Channel = #channel{name = ChannelName, pid = ChannelPid, users = [ClientPid]},
    State#server_state{channels = [Channel | State#server_state.channels]},
