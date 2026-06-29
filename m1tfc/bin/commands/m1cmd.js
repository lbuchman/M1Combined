'use strict';

const delay = require('delay');
const IctTestRunner = require('../../tests/ictTestRunner');
const targetICTLink = require('../../src/m1ICTLink');
const { runCommand } = require('../commandRunner');

function register(program) {
    program
        .command('m1cmd')
        .description('execute M1 bootstrap raw command, make sure to run m1dfu command before ')
        .option('-c, --command <string>', 'M1-3200 command, make sure to inclose the command in ""')
        .action(options => {
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
                await targetICTLink.initSerial(configData.m1SerialDev, 115200, logfile);
                const output = await targetICTLink.sendCommand(options.command);
                logfile.log(JSON.stringify(output));
                process.exit(0);
            };
            action.requiresSerial = false;
            runCommand(options, 'm1cmd', null, action);
        });
}

module.exports = {
    register
};
