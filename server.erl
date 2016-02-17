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
    PossibleCollisons = [ {Pid, Nick} || {Pid, {Nick, _}} <- State#server_state.users, Pid == NewPid orelse Nick == NewNick],
    case PossibleCollisons of
        % No collissions means we add the user to our list and reply with ok
        [] ->
            CurrentUsers = State#server_state.users,
            NewState = State#server_state{users = [ {NewPid, {NewNick, []}} |
                                                    CurrentUsers ]},
            io:fwrite("Users: ~p~n", [NewState#server_state.users]),
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
            {reply, ok, NewState}
    end;

handle(State, {join, Channel, Pid}) ->
    {Nick, Channels} = get_user(State, Pid),
    {Data, NewState} = case lists:member(Channel, Channels) of
        false ->
            NewUsers = [{Pid, {Nick, [Channel | Channels]}} | proplists:delete(Pid, State#server_state.users)],
            {ok, State#server_state{ users = NewUsers}};
        true ->
            {{error, user_already_joined}, State}
    end,

    {reply, Data, NewState};

handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi! You failed at pattern matching!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.

% Returns the user with the given Pid, or `undefined` if user is not connected
get_user(State, Pid) ->
    proplists:get_value(Pid, State#server_state.users).
