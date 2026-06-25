'use strict';

const delay = require('delay');
const IctTestRunner = require('../../tests/ictTestRunner');
const testBoardLink = require('../../src/testBoardLink');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, applyRuntime } = require('../commandSupport');

function register(program) {
    program
        .command('m1tbcmd')
        .description(
            'Execute M1 test board raw command (STM32MP1 core testing: voltages, programming, pogo pins)'
        )
        .option('-c, --command <string>', 'M1 test board command (enclose in quotes)')
        .action(async options => {
            const configData = await loadConfig();
            const logfile = console;
            applyRuntime(configData);

            if (!options.command) {
                logfile.error('Error: command option (-c) is required');
                process.exit(exitCodes.commandFailed);
            }

            try {
                const ictTestRunner = new IctTestRunner(
                    configData.ictFWFilePath,
                    configData.tolerance,
                    logfile,
                    configData,
                    false
                );
                await ictTestRunner.init(
                    configData.testBoardTerminalDev,
                    configData.serialBaudrate,
                    configData.m1SerialDev,
                    configData.serialBaudrate
                );
                await delay(400);

                const output = await testBoardLink.sendCommand(options.command);
                logfile.log(JSON.stringify(output));
                process.exit(0);
            } catch (err) {
                logfile.error(`M1 test board command failed: ${err.message}`);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};
