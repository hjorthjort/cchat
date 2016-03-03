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
%%
%% Pretty much all validation is made right here in the client and not on the
%% server. This makes it easier to make the program run concurrently, since
%% the server doesn't become as much of a bottle neck. Though one could argue
%% that it would be more "correct" to let the server handle the validation,
%% but this makes the code more complicated that it needs to be.

%% Connect to server
handle(State, {connect, Server}) ->
    case State#client_state.server of
        undefined ->
            ServerAtom = list_to_atom(Server),
            case catch genserver:request(ServerAtom, {connect, self(), State#client_state.nick}) of
                ok ->
                    NewState = State#client_state{server = ServerAtom},
                    {reply, ok, NewState};
                {error, user_already_connected} ->
                    {reply, {error, user_already_connected, "Your nick is already taken"}, State};
                {'EXIT', _Reason} ->
                    {reply, {error, server_not_reached, "Server not reached"}, State}
            end;
        _Server ->
            {reply, {error, user_already_connected, "Already connected to server"}, State}
    end;

%% Disconnect from server
handle(State, disconnect) ->
    case State#client_state.server of
        undefined ->
            {reply, {error, user_not_connected, "You are not connected to a server"}, State};
        Server ->
            case State#client_state.channels of
                [] ->
                    case catch genserver:request(Server, {disconnect, self()}) of
                        ok ->
                            NewState = State#client_state{server = undefined},
                            {reply, ok, NewState};
                        {'EXIT', _Reason} ->
                            {reply, {error, server_not_reached, "Server not reached"}, State}
                    end;
                [_H | _T] ->
                    {reply, {error, leave_channels_first, "Leave channels before disconnecting"}, State}
            end
    end;

% Join channel
handle(State, {join, ChannelName}) ->
    case channel_is_joined(State, ChannelName) of
        false ->
            case catch genserver:request(State#client_state.server, {join, ChannelName, self()}) of
                {'EXIT', _Reason} ->
                    {reply, {error, server_not_reached, "Server not reached"}, State};
                ChannelPid ->
                    NewState = add_channel(State, ChannelName, ChannelPid),
                    {reply, ok, NewState}
            end;
        true ->
            {reply, {error, user_already_joined, "Channel already joined"}, State}
    end;

%% Leave channel
handle(State, {leave, ChannelName}) ->
    case channel_is_joined(State, ChannelName) of
        true ->
            case catch genserver:request(State#client_state.server, {leave, ChannelName, self()}) of
                ok ->
                    NewState = remove_channel(State, ChannelName),
                    {reply, ok, NewState};
                {'EXIT', _Reason} ->
                    {reply, {error, server_not_reached, "Server not reached"}, State}
            end;
        false ->
            {reply, {error, user_not_joined, "Not in channel"}, State}
    end;

% Sending messages
handle(State, {msg_from_GUI, ChannelName, Message}) ->
    case channel_is_joined(State, ChannelName) of
        true ->
            % Here we send the message directly to the channel, bypassing the server completely.
            % This makes for excellent concurrency since the server doesn't become a bottle neck.
            #channel{pid = ChannelPid} = get_channel(State, ChannelName),
            genserver:request(ChannelPid, {send_message, #user{nick = State#client_state.nick, pid = self()}, Message}),
            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined, "Not in channel"}, State}
    end;

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

%% Incoming message from myself
handle(State, {incoming_msg, _Channel, Name, _Message}) when State#client_state.nick =:= Name ->
    {reply, ok, State};

%% Incoming message from someone else
handle(State = #client_state { gui = GUIName }, {incoming_msg, Channel, Name, Message}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name ++ "> " ++ Message}),
    {reply, ok, State};

handle(State, {job, {Fun, Ref, Input}}) ->
    Result = Fun(Input),
    NewState = State#client_state{job_results = [{Ref, Result} | State#client_state.job_results]},
    {reply, ok, NewState}.

%% -----------------------------------------------------------------------------

get_channel(State, ChannelName) ->
    case lists:filter(fun(Channel) -> Channel#channel.name =:= ChannelName end, State#client_state.channels) of
        [] ->
            undefined;
        [Channel | _T] ->
            Channel
    end.

%% -----------------------------------------------------------------------------

channel_is_joined(State, ChannelName) ->
    case get_channel(State, ChannelName) of
        undefined ->
            false;
        _Channel ->
            true
    end.

%% -----------------------------------------------------------------------------

remove_channel(State, ChannelName) ->
    Channels = State#client_state.channels,
    NewChannelList = lists:filter(fun(Channel) -> Channel#channel.name =/= ChannelName end, Channels),
    State#client_state{channels = NewChannelList}.

%% -----------------------------------------------------------------------------

add_channel(State, ChannelName, ChannelPid) ->
    NewChannel = #channel{name = ChannelName, pid = ChannelPid},
    OldChannelList = State#client_state.channels,
    State#client_state{channels = [NewChannel | OldChannelList]}.
