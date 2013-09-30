% module name
-module(symfony_port_demo).

% exported functions
-export([execute/2]).

% execute one command with specified parameters, sleep is used to prevent multiple initialization
% @param string Command
% @param string Parameters
execute(Command, Parameters) ->
    Process = symfony_port:open(),
    timer:sleep(10),
    Response = symfony_port:execute_command(Process, Command, Parameters),
    io:format("~s~n", [Response]),
    timer:sleep(10),
    symfony_port:close(Process).
