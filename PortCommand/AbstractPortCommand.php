<?php

namespace YsTools\ErlangPortBundle\PortCommand;

abstract class AbstractPortCommand implements PortCommandInterface
{
    /**
     * Method name that will receive parameters and process command
     */
    const CALLBACK_METHOD = 'processCommand';

    /**
     * Transfer control to method with signature that depends on number of argument of specific command
     *
     * {@inheritDoc}
     */
    public function execute(array $parameters)
    {
        return call_user_func_array(array($this, static::CALLBACK_METHOD), $parameters);
    }
}
