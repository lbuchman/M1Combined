'use strict';

const delay = require('delay');
const IctTestRunner = require('../../tests/ictTestRunner');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, applyRuntime } = require('../commandSupport');

function register(program) {
    program.command('m1dfu')
        .description('Start M1 in DFU mode and program bootstrap FW')
        .action(async () => {
            const configData = await loadConfig();
            const logfile = console;
            applyRuntime(configData);
            try {
                const ictTestRunner = new IctTestRunner(`${configData.mtfDir}/${configData.ictFWFilePath}`, configData.tolerance, logfile, configData, false);
                await ictTestRunner.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                await delay(400);
                await ictTestRunner.runTest(configData.programmingCommand, 'debug', 0, false, true);
                process.exit(0);
            }
            catch (err) {
                logfile.error(err);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};
