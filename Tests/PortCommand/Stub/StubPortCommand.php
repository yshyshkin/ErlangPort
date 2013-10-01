<?php

namespace YsTools\ErlangPortBundle\Tests\PortCommand\Stub;

use YsTools\ErlangPortBundle\PortCommand\AbstractPortCommand;

class StubPortCommand extends AbstractPortCommand
{
    /**
     * @param string $firstArgument
     * @param string $secondArgument
     */
    public function processCommand($firstArgument, $secondArgument)
    {
    }
}
