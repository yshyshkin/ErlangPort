<?php

namespace YsTools\ErlangPortBundle\Port;

interface PortCommandInterface
{
    /**
     * Unique command name
     *
     * @return string
     */
    public function getName();

    /**
     * Execute Erlang port command
     *
     * @param array $parameters
     * @return string
     */
    public function execute(array $parameters);
}
