<?php

namespace YsTools\ErlangPortBundle\Tests\PortCommand;

use YsTools\ErlangPortBundle\PortCommand\AbstractPortCommand;

class AbstractPortCommandTest extends \PHPUnit_Framework_TestCase
{
    public function testExecute()
    {
        $firstArgument = 'first_argument_string';
        $secondArgument = 'second_argument_string';
        $expectedResult = 'result_string';

        // StubPortCommand is a descendant of AbstractPortCommand with implemmented method processCommand
        $command = $this->getMockBuilder('YsTools\ErlangPortBundle\Tests\PortCommand\Stub\StubPortCommand')
            ->setMethods(array(AbstractPortCommand::CALLBACK_METHOD))
            ->getMock();
        $command->expects($this->once())
            ->method(AbstractPortCommand::CALLBACK_METHOD)
            ->with($firstArgument, $secondArgument)
            ->will($this->returnValue($expectedResult));

        /** @var AbstractPortCommand $command */
        $this->assertEquals($expectedResult, $command->execute(array($firstArgument, $secondArgument)));
    }
}
