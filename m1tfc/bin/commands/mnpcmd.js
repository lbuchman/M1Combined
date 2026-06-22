'use strict';

const exitCodes = require('../../src/exitCodes');
const mnpHwIo = require('../../tests/mnpHW');
const targetICTLink = require('../../src/m1ICTLink');
const { loadConfigData, createIctRunner } = require('../m1tfcShared');

module.exports = function registerMnpCmd(program) {
    program.command('mnpcmd <action> [sigNameOrTestpont] [value]')
        // eslint-disable-next-line
        .description('execute mnp IO commands\n\tCommands: [read, write, printio]\n\Example:\n\tm1test write WGD1_BPR 1\n\ttm1test read WGD2_D0_3V3 WGD1_BPR 0\n\nuse command printio to list testpoints and signames\n\nMake sure to execute m1dfu command before this command to load the FW')
        .action(async (readOrWrite, name, value) => {
            const configData = await loadConfigData();
            const logfile = console;
            let exitCode = exitCodes.normalExit;
            try {
                const command = mnpHwIo.getCommand(readOrWrite, name, value, logfile);
                await createIctRunner(configData, logfile);
                await targetICTLink.initSerial(configData.m1SerialDev, 115200, logfile);
                logfile.log(JSON.stringify(command));
                const output = await targetICTLink.sendCommand(command);
                logfile.log(JSON.stringify(output));
            }
            catch (err) {
                if (err.message === 'No Error') return;
                logfile.error(err);
                exitCode = exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};
