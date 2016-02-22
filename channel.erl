-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(ChannelName) ->
    #channel_state { name=ChannelName }.

%% -----------------------------------------------------------------------------

%% handle/2 handles requests from a server

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Join channel
%% Parameters in request:
%%   User: A user record for the joining user
handle(State, {join, User}) ->
    case is_user_in_channel(State, User) of
        true ->
            {reply, {error, user_already_joined}, State};
        false ->
            NewState = State#channel_state{ users = [User | State#channel_state.users] },
            {reply, ok, NewState}
    end;

%% Leave channel
%% Parameters in request:
%%   User: A user record for the leaving user
handle(State, {leave, User}) ->
    case is_user_in_channel(State, User) of
        false ->
            {reply, {error, user_not_joined}, State};
        true ->
            NewState = State#channel_state{ users = lists:delete(User, State#channel_state.users) },
            {reply, ok, NewState}
    end;

%% Send message
%% Parameters in request:
%%   Sender: A user record for the sending user
%%   Message: A string containing the message to send
handle(State, {send_message, Sender, Message}) ->
    io:fwrite("[channel] State: ~p~n", [State]),
    case is_user_in_channel(State, Sender) of
        false ->
            {reply, {error, user_not_joined}, State};
        true ->
            UsersToSendTo = lists:filter(fun(User) -> User#user.nick =/= Sender#user.nick end, State#channel_state.users),
            lists:foreach(fun(Receiver) -> send_message(State, Sender, Receiver, Message) end, UsersToSendTo),
            {reply, ok, State}
    end.

%% -----------------------------------------------------------------------------

%% Parameters:
%%   Sender: A user record for the sending user
%%   Receiver: A user record for the user that should receive the message
%%   Message: A string containing the message
send_message(State, Sender, Receiver, Message) ->
    genserver:request(Receiver#user.pid, {incoming_msg, State#channel_state.name, Sender#user.nick, Message}).

%% -----------------------------------------------------------------------------

%% Parameters:
%%   User: A user record for the user that we want to check for
is_user_in_channel(State, User) ->
    lists:member(User, State#channel_state.users).
