% Full open port command that should be executed in console
-define(PORT_COMMAND, "php /opt/www/symfony/app/console ystools:erlang-port").
-define(PORT_TIMEOUT, 60000).
-define(RESPONSE_PREFIX_OK, "ok:").
-define(RESPONSE_PREFIX_ERROR, "error:").
