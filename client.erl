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
handle(St, disconnect) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Join channel
handle(St, {join, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

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
