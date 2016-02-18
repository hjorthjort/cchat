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
    case get_user(State, Pid) of
        undefined ->
            {reply, {error, user_not_connected}, State};
        {_Nick, [_H | _T]} ->
            {reply, {error, leave_channels_first}, State};
        {_Nick, []} ->
            CurrentUsers = State#server_state.users,
            NewState = State#server_state{ users = proplists:delete(Pid, CurrentUsers)},
            io:fwrite("Users: ~p~n", [NewState#server_state.users]),
            {reply, ok, NewState}
    end;

handle(State, {join, Channel, Pid}) ->
    {Nick, Channels} = get_user(State, Pid),
    {Data, NewState} = case lists:member(Channel, Channels) of
        false ->
            UpdatedChannels = [Channel | Channels],
            NewUser = {Pid, {Nick, UpdatedChannels}},
            {ok, update_user(State, NewUser)};
        true ->
            {{error, user_already_joined}, State}
    end,
    io:fwrite("Users: ~p~n", [NewState#server_state.users]),
    {reply, Data, NewState};

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
        

handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi! You failed at pattern matching!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.

% Returns the user with the given Pid, or `undefined` if user is not connected
get_user(State, Pid) ->
    proplists:get_value(Pid, State#server_state.users).

% Updates a single user and returns the new server state
update_user(State, User) ->
    {Pid, _} = User,
    State#server_state{ users = [User | proplists:delete(Pid, State#server_state.users)]}.
