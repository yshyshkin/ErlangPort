% module name
-module(symfony_port).

% exported functions
-export([open/0, close/1, execute_command/3]).
-export([init/1]). % internal function, should not be executed externally

% configuration and interfaces
-include("symfony_port_config.hrl").
-include("symfony_port_interface.hrl").

% init process with open port command
% @return process
open() ->
    spawn(?MODULE, init, [?PORT_COMMAND]).

% stop dispatching
% @param Process Process
close(Process) ->
   Process ! stop.

% execute command on port
% @param process Process
% @param string Command
% @param string Parameters
% @return string
% @exit port_not_responding
execute_command(Process, Command, Parameters) ->
    PortMessage = io_lib:format("~s ~s~n", [Command, Parameters]),
    Process ! #call{process=self(), message=PortMessage},
    receive
        {Process, Response} ->
            Response
    after ?PORT_TIMEOUT ->
        exit(port_not_responding)
    end.

% init port with specified command
% @param string PortCommand
% @return process
init(PortCommand) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, PortCommand}, [stream, exit_status]),
    dispatch(self(), Port).

% dispatch requests and responses
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
