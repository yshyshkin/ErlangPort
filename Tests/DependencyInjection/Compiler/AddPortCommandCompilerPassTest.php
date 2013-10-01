<?php

namespace YsTools\ErlangPortBundle\Tests\DependencyInjection\Compiler;

use YsTools\ErlangPortBundle\DependencyInjection\Compiler\AddPortCommandCompilerPass;

class AddPortCommandCompilerPassTest extends \PHPUnit_Framework_TestCase
{
    public function testProcess()
    {
        $namedCommandServiceId = 'test.named_command';
        $namedCommandAlias = 'named_command';
        $namedCommandDefinition = $this->getMockBuilder('Symfony\Component\DependencyInjection\Definition')
            ->disableOriginalConstructor()
            ->getMock();

        $unnamedCommandServiceId = 'test.unnamed_command';
        $unnamedCommandDefinition = $this->getMockBuilder('Symfony\Component\DependencyInjection\Definition')
            ->disableOriginalConstructor()
            ->getMock();

        $taggedServices = array(
            $namedCommandServiceId => array(array('alias' => $namedCommandAlias)),
            $unnamedCommandServiceId => array(array())
        );

        $registryDefinition = $this->getMockBuilder('Symfony\Component\DependencyInjection\Definition')
            ->disableOriginalConstructor()
            ->setMethods(array('addMethodCall'))
            ->getMock();
        $registryDefinition->expects($this->at(0))
            ->method('addMethodCall')
            ->with('addPortCommand', array($namedCommandAlias, $namedCommandDefinition));
        $registryDefinition->expects($this->at(1))
            ->method('addMethodCall')
            ->with('addPortCommand', array($unnamedCommandServiceId, $unnamedCommandDefinition));

        $container = $this->getMockBuilder('Symfony\Component\DependencyInjection\ContainerBuilder')
            ->disableOriginalConstructor()
            ->setMethods(array('getDefinition', 'findTaggedServiceIds'))
            ->getMock();
        $container->expects($this->once())
            ->method('findTaggedServiceIds')
            ->with(AddPortCommandCompilerPass::PORT_COMMAND_TAG)
            ->will($this->returnValue($taggedServices));
        $container->expects($this->any())
            ->method('getDefinition')
            ->will(
                $this->returnValueMap(
                    array(
                        array(AddPortCommandCompilerPass::PORT_COMMAND_REGISTRY, $registryDefinition),
                        array($namedCommandServiceId, $namedCommandDefinition),
                        array($unnamedCommandServiceId, $unnamedCommandDefinition),
                    )
                )
            );

        $compiler = new AddPortCommandCompilerPass();
        $compiler->process($container);
    }
}
