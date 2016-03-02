% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

send_job(Server, F, Inputs) ->
    ClientPids = genserver:request(list_to_atom(Server), get_user_pids),
    ReferencesAndInputs = lists:zip(lists:seq(1, length(Inputs)), Inputs),
    ReferencesAndInputs.

wait_for_responses(Results, []) ->
    Results;

wait_for_responses(Results, [Reference | Tail]) ->
    receive {Reference, Result} ->
        wait_for_responses([Result | Results], Tail)
    end.
