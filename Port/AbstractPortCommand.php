<?php

namespace YsTools\ErlangPortBundle\Port;

abstract class AbstractPortCommand implements PortCommandInterface
{
    /**
     * Method name that will receive parameters and process command
     *
     * @var string
     */
    protected $callbackFunction = 'processCommand';

    /**
     * Transfer control to method with signature that depends on number of argument of specific command
     *
     * {@inheritDoc}
     */
    public function execute(array $parameters)
    {
        return call_user_func_array(array($this, $this->callbackFunction), $parameters);
    }
}
