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
    case State#client_state.server of
        undefined ->
            ServerAtom = list_to_atom(Server),
            case request(ServerAtom, {connect, self(), State#client_state.nick}) of
                ok ->
                    NewState = State#client_state{server = ServerAtom},
                    {reply, ok, NewState};
                Response ->
                    {reply, Response, State}
            end;
        _Server ->
            {reply, {error, user_already_connected, "Already connected to server"}, State}
    end;

%% Disconnect from server
handle(State, disconnect) ->
    case State#client_state.channels of
        [] ->
            case request(State#client_state.server, {disconnect, self()}) of
                ok ->
                    NewState = State#client_state{server = undefined},
                    {reply, ok, NewState};
                Response ->
                    {reply, Response, State}
            end;
        [_H | _T ] ->
            {reply, {error, leave_channels_first, "Leave channels before disconnecting"}, State}
    end;

% Join channel
handle(State, {join, Channel}) ->
    case lists:member(Channel, State#client_state.channels) of
        false ->
            case request(State#client_state.server, {join, Channel, self()}) of
                ok ->
                    NewState = State#client_state{ channels = [ Channel | State#client_state.channels ]},
                    {reply, ok, NewState};
                Response ->
                    {reply, Response, State}
            end;
        true ->
            {reply, {error, user_already_joined, "Channel already joined"}, State}
    end;

%% Leave channel
handle(State, {leave, Channel}) ->
    case lists:member(Channel, State#client_state.channels) of
        true ->
            case request(State#client_state.server, {leave, Channel, self()}) of
                ok ->
                    NewState = State#client_state{ channels = lists:delete(Channel, State#client_state.channels) },
                    {reply, ok, NewState};
                Response ->
                    {reply, Response, State}
            end;
        false ->
            {reply, {error, user_not_joined, "Not in channel"}, State}
    end;

% Sending messages
handle(State, {msg_from_GUI, Channel, Message}) ->
    case lists:member(Channel, State#client_state.channels) of
        true ->
            case request(State#client_state.server, {send_message, Channel, Message, self()}) of
                ok ->
                    {reply, ok, State};
                Response ->
                    {reply, Response, State}
            end;
        false ->
            {reply, {error, user_not_joined, "Not in channel"}, State}
    end;

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
request(Server, Request) ->
    io:fwrite("Sending request ~p~n", [Request]),
    % Check the state of the server to see if the client is connected
    case Server of
        % If server is undefined the user is not connected and we respond with an error
        undefined ->
            io:fwrite("Server undefined~n", []),
            {error, user_not_connected, "Not connected to server"};
        % Otherwise we send the request
        _ ->
            case catch genserver:request(Server, Request) of
                % If the server responds with `ok` we return the new state
                ok ->
                    io:fwrite("Response ok~n", []),
                    ok;
                % If the request exits in any way the server could not be reached
                {'EXIT', _Reason} ->
                    io:fwrite("EXIT reason ~p~n", [_Reason]),
                    {error, server_not_reached, "Server not reached"}
            end
    end.
