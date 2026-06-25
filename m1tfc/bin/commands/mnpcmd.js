'use strict';

const delay = require('delay');
const IctTestRunner = require('../../tests/ictTestRunner');
const mnpHwIo = require('../../tests/mnpHW');
const targetICTLink = require('../../src/m1ICTLink');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, applyRuntime } = require('../commandSupport');

function register(program) {
    program.command('mnpcmd <action> [sigNameOrTestpont] [value]')
        .description('execute mnp IO commands\n\tCommands: [read, write, printio]\nExample:\n\tm1test write WGD1_BPR 1\n\ttm1test read WGD2_D0_3V3 WGD1_BPR 0\n\nuse command printio to list testpoints and signames\n\nMake sure to execute m1dfu command before this command to load the FW')
        .action(async(readOrWrite, name, value) => {
            const configData = await loadConfig();
            const logfile = console;
            applyRuntime(configData);
            try {
                const command = mnpHwIo.getCommand(readOrWrite, name, value, logfile);
                const ictTestRunner = new IctTestRunner(`${configData.mtfDir}/${configData.ictFWFilePath}`, configData.tolerance, logfile, configData, false);
                await ictTestRunner.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                await delay(400);
                await targetICTLink.initSerial(configData.m1SerialDev, 115200, logfile);
                logfile.log(JSON.stringify(command));
                const output = await targetICTLink.sendCommand(command);
                logfile.log(JSON.stringify(output));
                process.exit(0);
            } catch (err) {
                if (err.message === 'No Error') {
                    return;
                }
                logfile.error(err);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};
