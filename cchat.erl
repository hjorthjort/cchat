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
    % Create a unique reference for every input value
    TasksAndClients = assign_tasks(ClientPids, Inputs),
    lists:foreach(fun(Element) -> give_task_to_client(Element, F) end, TasksAndClients),
    wait_for_responses([], TasksAndClients).

give_task_to_client({Client, {Ref, Input}}, F) ->
    io:format("~p F: ~p ~n", [{Client,{Ref, Input}}, F]),
    spawn(genserver, request, [Client, {job, {F, Ref, Input}}]).

assign_tasks([], _) ->
    [];

assign_tasks(Users, Tasks) ->
    [{lists:nth(((N-1) rem length(Users)) + 1, Users), {make_ref(), Task}}
        || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks)].

wait_for_responses(Results, []) ->
    lists:reverse(Results);

wait_for_responses(Results, [ {Client, {Reference, _Input}} | Tail]) ->
    Result = genserver:request(Client, {pop_result, Reference}, infinite),
    wait_for_responses([Result | Results], Tail).
