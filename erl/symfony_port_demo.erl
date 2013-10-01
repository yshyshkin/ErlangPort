% Module name
-module(symfony_port_demo).

% Exported functions
-export([execute/2, execute/1]).

% Execute one command with specified parameters, sleep is used to prevent multiple initialization
% @param string Name
% @param string Parameters
execute(Name, Parameters) ->
    Process = symfony_port:open(),
    timer:sleep(10),
    Response = symfony_port:execute_command(Process, Name, Parameters),
    io:format("~s~n", [Response]),
    timer:sleep(10),
    symfony_port:close(Process).

% Shorter form of execute/2
% @param string Name
execute(Name) ->
    execute(Name, []).
