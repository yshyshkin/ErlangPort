<?php

namespace YsTools\ErlangPortBundle\Tests\PortCommand;

use YsTools\ErlangPortBundle\PortCommand\PortCommandRegistry;
use YsTools\ErlangPortBundle\PortCommand\PortCommandInterface;

class PortCommandRegistryTest extends \PHPUnit_Framework_TestCase
{
    /**
     * @var PortCommandRegistry
     */
    protected $registry;

    protected function setUp()
    {
        $this->registry = new PortCommandRegistry();
    }

    protected function tearDown()
    {
        unset($this->registry);
    }

    public function testAddPortCommand()
    {
        $commandName = 'test_command';
        $command = $this->createPortCommand();

        $this->registry->addPortCommand($commandName, $command);
        $this->assertAttributeEquals(array($commandName => $command), 'commands', $this->registry);
    }

    /**
     * @expectedException \LogicException
     * @expectedExceptionMessage Port command "test_command" has already defined in registry
     */
    public function testAddPortCommandException()
    {
        $commandName = 'test_command';
        $command = $this->createPortCommand();

        $this->registry->addPortCommand($commandName, $command);
        $this->registry->addPortCommand($commandName, $command);
    }

    public function testHasPortCommand()
    {
        $commandName = 'test_command';
        $command = $this->createPortCommand();

        $this->assertFalse($this->registry->hasPortCommand($commandName));
        $this->registry->addPortCommand($commandName, $command);
        $this->assertTrue($this->registry->hasPortCommand($commandName));
    }

    public function testGetPortCommand()
    {
        $commandName = 'test_command';
        $command = $this->createPortCommand();

        $this->registry->addPortCommand($commandName, $command);
        $this->assertEquals($command, $this->registry->getPortCommand($commandName));
    }

    /**
     * @expectedException \LogicException
     * @expectedExceptionMessage Port command "test_command" can not be found
     */
    public function testGetPortException()
    {
        $this->registry->getPortCommand('test_command');
    }

    /**
     * @return PortCommandInterface
     */
    protected function createPortCommand()
    {
        return $this->getMockBuilder('YsTools\ErlangPortBundle\PortCommand\PortCommandInterface')
            ->getMockForAbstractClass();
    }
}
