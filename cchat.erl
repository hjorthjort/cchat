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
    TasksAndClients = assign_tasks(ClientPids, ReferencesAndInputs),
    ReturnPid = self(),
    lists:foreach(fun({Client, {Ref, Input}}) -> spawn(fun() -> ReturnPid ! genserver:request(Client, {job, {F, Ref, Input}}) end) end, TasksAndClients),
    wait_for_responses([], ReferencesAndInputs).

%cchat:send_job("shire", fun(X) -> X*2 end, [11,12,13])

assign_tasks([], _) ->
    [];

assign_tasks(Users, Tasks) ->
      [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
      || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].

wait_for_responses(Results, []) ->
    Results;

wait_for_responses(Results, [Reference | Tail]) ->
    receive {Reference, Result} ->
        wait_for_responses([Result | Results], Tail)
    end.
