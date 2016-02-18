-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_state { nick = Nick, gui = GUIName }.

%% ---------------------------------------------------------------------------

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
    request(State#client_state.server, {join, Channel, self()}, State, State);

%% Leave channel
handle(State, {leave, Channel}) ->
    request(State#client_state.server, {leave, Channel, self()}, State, State);

% Sending messages
handle(State, {msg_from_GUI, Channel, Message}) ->
    request(State#client_state.server, {send_message, Channel, Message, self()}, State, State);

%% Get current nick
handle(State, whoami) ->
    {reply, State#client_state.nick, State};

%% Change nick
handle(State, {nick, Nick}) ->
    {Data, NewState} = case State#client_state.server of
        undefined ->
            {ok, State#client_state{nick = Nick}};
        _ ->
            {{error, user_already_connected, "Can't change nick when connected to a server"}, State}
    end,
    {reply, Data, NewState};

%% Incoming message
handle(State = #client_state { gui = GUIName }, {incoming_msg, Channel, Name, Message}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name ++ "> " ++ Message}),
    {reply, ok, State}.

request(Server, Request, OldState, NewState) ->
    io:fwrite("Client: Sending request ~p~n", [Request]),
    % Check the state of the server to see if the client is connected
    {Data, State} = case Server of
        % If server is undefined the user is not connected and we respond with an error
        undefined ->
            io:fwrite("Client: Server undefined~n"),
            {{error, user_not_connected, "Not connected to server"}, OldState};
        % Otherwise we send the request
        _ ->
            case catch genserver:request(Server, Request) of
                % If the server respons with ok we send back the new state
                ok ->
                    io:fwrite("Client: Response ok~n"),
                    {ok, NewState};
                % If the server responds with an error we leave the state unchanged and send back the error
                {error, user_already_connected} ->
                    io:fwrite("Client: User already connected~n"),
                    {{error, user_already_connected, "Already connected to server"}, OldState};
                {error, user_not_connected} ->
                    io:fwrite("Client: User not connected~n"),
                    {{error, user_not_connected, "Not connected to server"}, OldState};
                {error, leave_channels_first} ->
                    io:fwrite("Client: Leave channels first~n"),
                    {{error, leave_channels_first, "Leave channels before disconnecting"}, OldState};
                {error, user_already_joined} ->
                    io:fwrite("Client: User already joined~n"),
                    {{error, user_already_joined, "Channel already joined"}, OldState};
                {error, user_not_joined} ->
                    io:fwrite("Client: User not joined~n"),
                    {{error, user_not_joined, "Not in channel"}, OldState};
                % If the request exits in any way the server could not be reached
                {'EXIT', _Reason} ->
                    io:fwrite("Client: EXIT reason ~p~n", [_Reason]),
                    {{error, server_not_reached, "Server not reached"}, OldState}
            end
    end,
    {reply, Data, State}.
