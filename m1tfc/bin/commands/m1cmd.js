'use strict';

const exitCodes = require('../../src/exitCodes');
const targetICTLink = require('../../src/m1ICTLink');
const { loadConfigData, createIctRunner } = require('../m1tfcShared');

module.exports = function registerM1Cmd(program) {
    program.command('m1cmd')
        .description('execute M1 bootstrap raw command, make sure to run m1dfu command before ')
        .option('-c, --command <string>', 'M1-3200 command, make sure to inclose the command in ""')
        .action(async (options) => {
            const configData = await loadConfigData();
            const logfile = console;
            let exitCode = exitCodes.normalExit;
            try {
                await createIctRunner(configData, logfile);
                await targetICTLink.initSerial(configData.m1SerialDev, 115200, logfile);
                const output = await targetICTLink.sendCommand(options.command);
                logfile.log(JSON.stringify(output));
            }
            catch (err) {
                logfile.error(err);
                exitCode = exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};
