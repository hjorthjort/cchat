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

%% Sends jobs to all clients connected to the specified server. A job consists
%% of running a function with a specified input.
%% Parameters:
%%     Server - The name of the server to use
%%     F - The function that we want to run
%%     Inputs - A list of inputs that we want to calculate with the specified function
send_job(Server, F, Inputs) ->
    ClientPids = genserver:request(list_to_atom(Server), get_user_pids),
    % Create a unique reference for every input value
    Tasks = assign_tasks(ClientPids, Inputs),
    lists:foreach(fun(Task) -> give_task_to_client(Task, F) end, Tasks),
    References = [ Ref || {_Client, {Ref, _Input}} <- Tasks ],
    wait_for_responses([], References).

%% Sends a request to calculate a task. A task should be pre-assigned to a
%% specific client.
%% Paremeters:
%%     Task {Client, {Ref, Input}} - The task that we want to compute. A task
%%                                   consists of a client, a reference and an input
%%     F - The function that should be used to compute the result of the task input
give_task_to_client({Client, {Ref, Input}}, F) ->
    ReturnPid = self(),
    spawn(fun() -> ReturnPid ! genserver:request(Client, {job, {F, Ref, Input}}, infinity) end).

%% Assigns an input to a client, and generates a unique reference that can be used
%% to identify the result of that particular input.
%% Parameters:
%%     Users - A list of pids to client processes
%%     Tasks {Ref, Input} - A list of tasks consiting of references and inputs
assign_tasks([], _) ->
    [];

assign_tasks(Users, Tasks) ->
    [{lists:nth(((N-1) rem length(Users)) + 1, Users), {make_ref(), Task}}
        || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks)].

%% ------------------------------------------------------------------------------
%% Wait for responses to a set of messages defined by a list of references.
%% Returns a list of responses in messages of the form {Reference, Response}.
%% The order in the returned list matches the order in the list of references.
%%
%% Parameters:
%%      Results: Already finished results, in reverse order
%%      References: A list of references for which we will get corresponding
%%          messages

wait_for_responses(Responses, []) ->
    lists:reverse(Responses);

wait_for_responses(Responses, [ Reference | UnhandledReferences ]) ->
    receive
        {Reference, Response} ->
            wait_for_responses([Response | Responses], UnhandledReferences)
    end.
