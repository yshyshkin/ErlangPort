<?php

namespace YsTools\ErlangPortBundle\DependencyInjection\Compiler;

use Symfony\Component\DependencyInjection\ContainerBuilder;
use Symfony\Component\DependencyInjection\Compiler\CompilerPassInterface;

class AddPortCommandCompilerPass implements CompilerPassInterface
{
    const PORT_COMMAND_TAG      = 'ys_tools.erlang_port.port_command';
    const PORT_COMMAND_REGISTRY = 'ys_tools.erlang_port.port_command.registry';

    /**
     * @param ContainerBuilder $container
     */
    public function process(ContainerBuilder $container)
    {
        $registryDefinition = $container->getDefinition(self::PORT_COMMAND_REGISTRY);

        foreach ($container->findTaggedServiceIds(self::PORT_COMMAND_TAG) as $id => $attributes) {
            foreach ($attributes as $eachTag) {
                if (!empty($eachTag['alias'])) {
                    $commandName = $eachTag['alias'];
                } else {
                    $commandName = $id;
                }
                $registryDefinition->addMethodCall(
                    'addPortCommand',
                    array($commandName, $container->getDefinition($id))
                );
            }
        }
    }
}
