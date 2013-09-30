<?php

namespace YsTools\ErlangPortBundle\Command;

use Symfony\Bundle\FrameworkBundle\Command\ContainerAwareCommand;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

use YsTools\ErlangPortBundle\PortCommand\PortCommandRegistry;
use YsTools\ErlangPortBundle\DependencyInjection\Compiler\AddPortCommandCompilerPass;

/**
 * Command should be executed in CLI environment to allow interaction with Erlang
 */
class ConsolePortCommand extends ContainerAwareCommand
{
    const RESPONSE_PREFIX_OK = 'ok:';
    const RESPONSE_PREFIX_ERROR = 'error:';

    const EXIT_COMMAND = 'exit';

    /**
     * @var PortCommandRegistry
     */
    protected $portCommandRegistry;

    protected function configure()
    {
        $this
            ->setName('ystools:erlang-port')
            ->setDescription('Entry point for Erlang port to execute commands');
    }

    /**
     * @param InputInterface $input
     * @param OutputInterface $output
     * @return int
     */
    protected function execute(InputInterface $input, OutputInterface $output)
    {
        while ($string = fgets(STDIN)) {
            try {
                // extract name and parameters
                $string = rtrim($string, "\r\n");
                $parameters = explode(' ', $string);
                $name = array_shift($parameters);

                // execute command
                if ($name == self::EXIT_COMMAND) {
                    if ($this->getPortCommandRegistry()->hasPortCommand($name)) {
                        $result = $this->executePortCommand($name, $parameters);
                    } else {
                        $result = $name;
                    }
                } else {
                    $result = $this->executePortCommand($name, $parameters);
                }

                // return result
                $result = str_replace("\n", ' ', $result); // result must be in one line
                $output->writeln(self::RESPONSE_PREFIX_OK . $result);

                // process exit
                if ($name == self::EXIT_COMMAND) {
                    break;
                }
            } catch (\Exception $exception) {
                $output->writeln(self::RESPONSE_PREFIX_ERROR . $exception->getMessage());
            }
        }

        return 0;
    }

    /**
     * @param string $name
     * @param array $parameters
     * @return string
     */
    protected function executePortCommand($name, array $parameters)
    {
        $portCommand = $this->getPortCommandRegistry()->getPortCommand($name);
        $commandResult = $portCommand->execute($parameters);

        return $commandResult;
    }

    /**
     * @return PortCommandRegistry
     */
    protected function getPortCommandRegistry()
    {
        if (!$this->portCommandRegistry) {
            $this->portCommandRegistry = $this->getContainer()->get(AddPortCommandCompilerPass::PORT_COMMAND_REGISTRY);
        }

        return $this->portCommandRegistry;
    }
}
