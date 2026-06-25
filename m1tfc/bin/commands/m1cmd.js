'use strict';

const delay = require('delay');
const IctTestRunner = require('../../tests/ictTestRunner');
const targetICTLink = require('../../src/m1ICTLink');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, applyRuntime } = require('../commandSupport');

function register(program) {
    program.command('m1cmd')
        .description('execute M1 bootstrap raw command, make sure to run m1dfu command before ')
        .option('-c, --command <string>', 'M1-3200 command, make sure to inclose the command in ""')
        .action(async(options) => {
            const configData = await loadConfig();
            const logfile = console;
            applyRuntime(configData);
            try {
                const ictTestRunner = new IctTestRunner(`${configData.mtfDir}/${configData.ictFWFilePath}`, configData.tolerance, logfile, configData, false);
                await ictTestRunner.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                await delay(400);
                await targetICTLink.initSerial(configData.m1SerialDev, 115200, logfile);
                const output = await targetICTLink.sendCommand(options.command);
                logfile.log(JSON.stringify(output));
                process.exit(0);
            } catch (err) {
                logfile.error(err);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};
