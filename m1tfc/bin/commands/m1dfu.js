'use strict';

const delay = require('delay');
const IctTestRunner = require('../../tests/ictTestRunner');
const { runCommand } = require('../commandRunner');

function register(program) {
    program
        .command('m1dfu')
        .description('Start M1 in DFU mode and program bootstrap FW')
        .action(options => {
            // Note: `executionFn` doesn't require serial
            const action = async (configData, logfile, _db) => {
                const ictTestRunner = new IctTestRunner(
                    `${configData.mtfDir}/${configData.ictFWFilePath}`,
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
                await ictTestRunner.runTest(configData.programmingCommand, 'debug', 0, false, true);
                process.exit(0);
            };
            action.requiresSerial = false; // Add property to say this command bypasses runCommand serial assertion
            runCommand(options, 'm1dfu', null, action);
        });
}

module.exports = {
    register
};
