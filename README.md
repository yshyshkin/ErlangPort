Symfony 2 Erlang Port Bundle
----------------------------

This bundle provides functionality to allow interaction between Erlang programs and Symfony application
using Erlang ports. Bundle has library of functions to work with port from the Erlang side and CLI command
and port command registry to work with registered port commands from the Symfony side.

* [Installation](#installation)
* [Erlang side](#erlang-side)
* [Symfony side](#symfony-side)
* [Example of usage](#example-of-usage)


### Installation

To install bundle you need to add string `"ystools/erlang-port-bundle": "dev-master"` to require section
of Symfony's main composer.json and run `composer update`.


### Erlang side

#### Library

There is the library [symfony\_port.erl](./erl/symfony_port.erl) that provides list of functions that should be used
to work with Symfony port:

* **open()** - create and open Symfony port, send "init" command to port, returns process identifier;
* **execute\_command(Process, Name, Parameters)** - functions that execute port command _Name_ for process _Process_
with list of parameters _Parameters_ (parameters are optional), returns string with command execution result;
* **close(Process)** - stop and close port identified by process _Process_, send "exit" command to port.

By default all Erlang files are available in directory [erl](./erl), but after installation all required files
should be copied to other location where they should be compiled and used.

#### Configuration

Configuration of port is stored in file [symfony\_port\_config.hrl](./erl/symfony_port_config.hrl)
that contains following parameters (macroses):

* **PORT\_CLI\_COMMAND** - full command line to start Erlang port on the Symfony side
(f.e. `php /opt/www/symfony/app/console ystools:erlang-port`);
* **PORT\_TIMEOUT** - port command execution timeout (maximum execution time), when this time is up port will be closed,
default value is 60 sec;
* **INIT\_COMMAND** - name of the command that will be sent to port right after port opening,
default value is `init`;
* **EXIT\_COMMAND** - name of the command that will be sent to port right before port closing,
default value is `exit`;
* **RESPONSE\_PREFIX\_OK** - response prefix that is used to identifier response as successful,
default value is `ok:`;
* **RESPONSE\_PREFIX\_ERROR** - response prefix that is used to identifier response as failed,
default value is `error:`;


### Symfony side

Bundle provides [PortCommandInterface](./PortCommand/PortCommandInterface.php) and
[AbstractPortCommand](./PortCommand/AbstractPortCommand.php) that should be used to implement port commands.
Each port command must be registered in DI container as a service with tag `ys_tools.erlang_port.port_command` -
tag alias is a command name.

Main entry point for Erlang is a console command `ystools:erlang-port` that should be specified as CLI command
on the Erlang side. This command also can be used to debug existing commands - all you need is to start it
and enter command name and parameters.

### Example of usage

Let's assume that we need to implement port command that will calculate SHA-1 hash of specified string.
We need to follow a few steps.

#### 1. Create port command class

Port command class must implement PortCommandInterface. Let's use existing AbstractPortCommand
that already implements this interface to create a command class:

```php
use YsTools\ErlangPortBundle\PortCommand\AbstractPortCommand;

class Sha1Calculator extends AbstractPortCommand
{
    /**
     * @param string $string
     * @return string
     */
    public function processCommand($string)
    {
        return sha1($string);
    }
}
```

Here _processCommand_ is a default function name used to execute command. If you want to change it you should to
override constant CALLBACK_METHOD.

#### 2. Register port command as DI service with tag

Now we need to tell that this class should be used as Erlang port command. To do that we need to register it
as a service with specific tag:

```
parameters:
    acme.demo.port_command.sha1_calculator.class: Acme\DemoBundle\PortCommand\Sha1Calculator

services:
    acme.demo.port_command.sha1_calculator:
        class: %acme.demo.port_command.sha1_calculator.class%
        tags:
            - { name: ys_tools.erlang_port.port_command, alias: calculate_sha1 }
```

Let's check that command is available and works. To do that we can manually start port command and enter required data:

```
login@host:/opt/www/symfony$ app/console ystools:erlang-port
calculate_sha1 qwerty
ok:b1b3773a05c0ed0176787a4f1574ff0075f7521e
exit
ok:exit
```

As you can see, command returns result with a response status prefix. Command `exit` is used to stop port process.

#### 3. Compile Erlang libraries

To work with Erlang code we need to compile existing library. Also bundle provides library
[symfony\_port\_demo.erl](./erl/symfony_port_demo.erl) that allow us to test command without additional Erlang code.
Let's compile libraries `symfony_port` and `symfony_port_demo`:

```
login@host:/opt/www/symfony/src/YsTools/ErlangPortBundle/erl$ erl
Eshell V5.10.3  (abort with ^G)
1> c(symfony_port).
{ok,symfony_port}
2> c(symfony_port_demo).
{ok,symfony_port_demo}
```

After compilation for each library there must be appeared file with extension *.beam - these are compiled libraries.

#### 4. Start port and execute command

Library `symfony_port_demo` provides function **execute(Name, Parameters)** that automatically opens port,
executes a command _Name_ with _Parameters_, prints result and closes port. Let's do it:

```
login@host:/opt/www/symfony/src/YsTools/ErlangPortBundle/erl$ erl
Eshell V5.10.3  (abort with ^G)
1> symfony_port_demo:execute("calculate_sha1", ["qwerty"]).
b1b3773a05c0ed0176787a4f1574ff0075f7521e
stop
```

As you can see, port command was successfully executed and it returned valid result.
