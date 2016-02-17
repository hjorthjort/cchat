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
    {Data, NewState} = case catch genserver:request(ServerAtom, { connect, self(), State#client_state.nick }) of
        ok ->
            {ok, State#client_state{server = ServerAtom}};
        {error, user_already_connected} ->
            {{error, user_already_connected, "Already connected to server"}, State};
        {'EXIT', _} ->
            {{error, server_not_reached, "Server not reached"}, State}
    end,
    {reply, Data, NewState};

%% Disconnect from server
handle(State, disconnect) ->
    {Data, NewState} = case catch genserver:request(State#client_state.server, { disconnect, self() }) of
        ok ->
            {ok, State#client_state{server = undefined}};
        {error, user_not_connected} ->
            {{error, user_not_connected, "Not connected to server"}, State};
        {'EXIT', _} ->
            {{error, server_not_reached, "Server not reached"}, State}
    end,
    {reply, Data, NewState};

% Join channel
handle(State, {join, Channel}) ->
    {Data, NewState} = case catch genserver:request(State#client_state.server, { join, Channel, self() }) of
        ok ->
            {ok, State};
        {error, user_already_joined} ->
            {{error, user_already_joined, "Channel already joined"}, State}
    end,
    {reply, Data, NewState};

%% Leave channel
handle(State, {leave, Channel}) ->
    {Data, NewState} = case catch genserver:request(State#client_state.server, { leave, Channel, self() }) of
        ok ->
            {ok, State};
        {error, user_not_joined} ->
            {{error, user_not_joined, "Not in channel"}, State}
    end,
    {reply, Data, NewState};

% Sending messages
handle(State, {msg_from_GUI, Channel, Message}) ->
    {Data, NewState} = case catch genserver:request(State#client_state.server, { send_message, Channel, Message, self() }) of
        ok ->
            {ok, State};
        {error, user_not_joined} ->
            {{error, user_not_joined, "Not in channel"}, State}
    end,
    {reply, Data, NewState};

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
handle(St = #client_state { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
