% Module name
-module(symfony_port).

% Exported functions
-export([open/0, close/1, execute_command/3, execute_command/2]).
-export([init/1]). % internal function, should not be executed externally

% Configuration and interfaces
-include("symfony_port_config.hrl").
-include("symfony_port_interface.hrl").

% Init process with open port command
% @return process
open() ->
    Process = spawn(?MODULE, init, [?PORT_CLI_COMMAND]),
    execute_command(Process, ?INIT_COMMAND),
    Process.

% Stop dispatching
% @param Process Process
close(Process) ->
    execute_command(Process, ?EXIT_COMMAND),
    Process ! stop.

% Execute command on port
% @param process Process
% @param string Name
% @param list Parameters
% @return string
% @exit port_not_responding
% @exit invalid_response
% @exit ErrorString
execute_command(Process, Name, Parameters) ->
    PortMessage = io_lib:format("~s ~s~n", [Name, string:join(Parameters, " ")]),
    Process ! #call{process=self(), message=PortMessage},
    receive
        {Process, Response} ->
            IsOkResponse = string:str(Response, ?RESPONSE_PREFIX_OK) == 1,
            IsErrorResponse = string:str(Response, ?RESPONSE_PREFIX_ERROR) == 1,
            if
                IsOkResponse ->
                    ResponseString = string:substr(Response, string:len(?RESPONSE_PREFIX_OK) + 1),
                    ResponseString;
                IsErrorResponse ->
                    ErrorString = string:substr(Response, string:len(?RESPONSE_PREFIX_ERROR) + 1),
                    exit(ErrorString);
                true ->
                    exit(invalid_response)
            end
    after ?PORT_TIMEOUT ->
        exit(port_not_responding)
    end.

% Shorter form of execute_command/3
% @param process Process
% @param string Name
% @param list Parameters
execute_command(Process, Name) ->
    execute_command(Process, Name, []).

% Init port with specified CLI command
% @param string PortCommand
% @return process
init(PortCommand) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, PortCommand}, [stream, exit_status]),
    dispatch(self(), Port).

% Dispatch requests and responses
% @param process Process
% @param process Port
% @exit normal
% @exit Reason
dispatch(Process, Port) ->
    receive
        #call{process=Caller, message=Message} ->
            Port ! {self(), {command, Message}},
            receive
                {Port, {data, Data}} ->
                    Response = string:strip(Data, right, $\n),
                    Caller ! {Process, Response}
            end,
            dispatch(Process, Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit(Reason)
    end.
