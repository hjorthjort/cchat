-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Name) ->
    #channel_state {
       name = Name
    }.

%% -----------------------------------------------------------------------------

%% handle/2 handles requests from a server

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Join channel. Allows same user to join multiple times, and thus assumes that
%% if the user can only join once, the client keeps track of this.
%% Parameters in request:
%%   User: A user record for the joining user
handle(State, {join, User}) ->
    {reply, ok, State#channel_state{ users = [User | State#channel_state.users] }};

%% Leave channel. If user is not in channel this has no effect.
%% Parameters in request:
%%   User: A user record for the leaving user
handle(State, {leave, User}) ->
    {reply, ok, State#channel_state{ users = lists:delete(User, State#channel_state.users) }};

%% Send message
%% Parameters in request:
%%   Sender: A user record for the sending user
%%   Message: A string containing the message to send
handle(State, {send_message, Sender, Message}) ->
    lists:foreach(fun(Receiver) -> send_message(State, Sender, Receiver,
                                                Message) end, State#channel_state.users),
    {reply, ok, State}.

%% -----------------------------------------------------------------------------

%% Parameters:
%%   Sender: A user record for the sending user
%%   Receiver: A user record for the user that should receive the message
%%   Message: A string containing the message
send_message(State, Sender, Receiver, Message) ->
    % When sending a message a new process is spawned for that specific message.
    % This is because we don't care about the response from the client, so we
    % don't want to block until we get it.
    spawn(genserver, request, [Receiver#user.pid, {incoming_msg,
                                                    State#channel_state.name,
                                                    Sender#user.nick, Message}]).
