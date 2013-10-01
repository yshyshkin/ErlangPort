% Full open port command that should be executed in console
-define(PORT_CLI_COMMAND, "php /opt/www/symfony/app/console ystools:erlang-port").

% Timeout (max execution time) for port command
-define(PORT_TIMEOUT, 60000).

% Сommand that is sent to port after port opening
-define(INIT_COMMAND, "init").

% Сommand that is sent to port before port closing
-define(EXIT_COMMAND, "exit").

% Response prefix of successful result
-define(RESPONSE_PREFIX_OK, "ok:").

% Response prefix of error result
-define(RESPONSE_PREFIX_ERROR, "error:").
