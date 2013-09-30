<?php

namespace YsTools\ErlangPortBundle\PortCommand;

class PortCommandRegistry
{
    /**
     * @var PortCommandInterface[]
     */
    protected $commands = array();

    /**
     * @param string $name
     * @param PortCommandInterface $command
     * @throws \LogicException
     */
    public function addPortCommand($name, PortCommandInterface $command)
    {
        if (array_key_exists($name, $this->commands)) {
            throw new \LogicException(
                sprintf('Port command "%s" has already defined in registry', $name)
            );
        }

        $this->commands[$name] = $command;
    }

    /**
     * @param string $name
     * @return bool
     */
    public function hasPortCommand($name)
    {
        return array_key_exists($name, $this->commands);
    }

    /**
     * @param string $name
     * @return PortCommandInterface
     * @throws \LogicException
     */
    public function getPortCommand($name)
    {
        if (!isset($this->commands[$name])) {
            throw new \LogicException(
                sprintf('Port command "%s" can not be found', $name)
            );
        }

        return $this->commands[$name];
    }
}
