<?php

namespace YsTools\ErlangPortBundle\PortCommand;

interface PortCommandInterface
{
    /**
     * Execute Erlang port command
     *
     * @param array $parameters
     * @return string
     */
    public function execute(array $parameters);
}
