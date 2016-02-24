-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_state { nick = Nick, gui = GUIName }.

%% -----------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(State, {connect, Server}) ->
    ServerAtom = list_to_atom(Server),
    request(ServerAtom, {connect, self(), State#client_state.nick}, State, State#client_state{server = ServerAtom});

%% Disconnect from server
handle(State, disconnect) ->
    request(State#client_state.server, {disconnect, self()}, State, State#client_state{server = undefined});

% Join channel
handle(State, {join, Channel}) ->
    case lists:member(Channel, State#client_state.channels) of
        false ->
            request(State#client_state.server, {join, Channel, self()}, State, State#client_state{ channels = [ Channel | State#client_state.channels ]});
        true ->
            {reply, {error, user_already_joined, "Channel already joined"}, State}
    end;

%% Leave channel
handle(State, {leave, Channel}) ->
    case lists:member(Channel, State#client_state.channels) of
        true ->
            request(State#client_state.server, {leave, Channel, self()}, State, State#client_state{ channels = lists:deletd(Channel, State#client_state.channels) });
        false ->
            {reply, {error, user_not_joined, "Not in channel"}, State}
    end;

% Sending messages
handle(State, {msg_from_GUI, Channel, Message}) ->
    request(State#client_state.server, {send_message, Channel, Message, self()}, State, State);

%% Get current nick
handle(State, whoami) ->
    io:fwrite("~p: whoami~n", [State#client_state.nick]),
    {reply, State#client_state.nick, State};

%% Change nick
handle(State, {nick, Nick}) ->
    io:fwrite("~p: Change nick to ~p~n", [State#client_state.nick, Nick]),
    {Data, NewState} = case State#client_state.server of
        undefined ->
            io:fwrite("~p: Nick changed to ~p~n", [State#client_state.nick, Nick]),
            {ok, State#client_state{nick = Nick}};
        _ ->
            io:fwrite("~p: User already connected~n", [State#client_state.nick]),
            {{error, user_already_connected, "Can't change nick when connected to a server"}, State}
    end,
    {reply, Data, NewState};

%% Incoming message
handle(State = #client_state { gui = GUIName }, {incoming_msg, Channel, Name, Message}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name ++ "> " ++ Message}),
    {reply, ok, State}.

%% -----------------------------------------------------------------------------

%% request/4 sends a request to a specified PID (Server). If the response from
%% the server is `ok` this function returns {reply, ok, NewState}. If the
%% response is an error it returns {reply, {error, atom, Message}, OldState}.
request(Server, Request, OldState, NewState) ->
    io:fwrite("~p: Sending request ~p~n", [OldState#client_state.nick, Request]),
    % Check the state of the server to see if the client is connected
    {Data, State} = case Server of
        % If server is undefined the user is not connected and we respond with an error
        undefined ->
            io:fwrite("~p: Server undefined~n", [OldState#client_state.nick]),
            {{error, user_not_connected, "Not connected to server"}, OldState};
        % Otherwise we send the request
        _ ->
            case catch genserver:request(Server, Request) of
                % If the server responds with `ok` we return the new state
                ok ->
                    io:fwrite("~p: Response ok~n", [OldState#client_state.nick]),
                    {ok, NewState};
                % If the server responds with an error we leave the state unchanged and return the error
                {error, user_already_connected} ->
                    io:fwrite("~p: User already connected~n", [OldState#client_state.nick]),
                    {{error, user_already_connected, "Already connected to server"}, OldState};
                {error, user_not_connected} ->
                    io:fwrite("~p: User not connected~n", [OldState#client_state.nick]),
                    {{error, user_not_connected, "Not connected to server"}, OldState};
                {error, leave_channels_first} ->
                    io:fwrite("~p: Leave channels first~n", [OldState#client_state.nick]),
                    {{error, leave_channels_first, "Leave channels before disconnecting"}, OldState};
                {error, user_already_joined} ->
                    io:fwrite("~p: User already joined~n", [OldState#client_state.nick]),
                    {{error, user_already_joined, "Channel already joined"}, OldState};
                {error, user_not_joined} ->
                    io:fwrite("~p: User not joined~n", [OldState#client_state.nick]),
                    {{error, user_not_joined, "Not in channel"}, OldState};
                % If the request exits in any way the server could not be reached
                {'EXIT', _Reason} ->
                    io:fwrite("~p: EXIT reason ~p~n", [OldState#client_state.nick, _Reason]),
                    {{error, server_not_reached, "Server not reached"}, OldState}
            end
    end,
    {reply, Data, State}.
