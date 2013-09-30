<?php

namespace YsTools\ErlangPortBundle;

use Symfony\Component\HttpKernel\Bundle\Bundle;
use Symfony\Component\DependencyInjection\ContainerBuilder;

use YsTools\ErlangPortBundle\DependencyInjection\Compiler\AddPortCommandCompilerPass;

class YsToolsErlangPortBundle extends Bundle
{
    /**
     * {@inheritdoc}
     */
    public function build(ContainerBuilder $container)
    {
        $container->addCompilerPass(new AddPortCommandCompilerPass());
    }
}
