'use strict';

const delay = require('delay');
const testBoardLink = require('../../src/testBoardLink');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, applyRuntime } = require('../commandSupport');

function normalizeState(state) {
    const value = String(state || '').trim().toLowerCase();
    if (value === 'on') return true;
    if (value === 'off' || value === 'auto') return false;
    return null;
}

function register(program) {
    program
        .command('power')
        .description('Set UUT target power on or off from the test fixture')
        .requiredOption('--state <on|off|auto>', 'target power state')
        .action(async options => {
            const configData = await loadConfig();
            const logfile = console;
            const powerOn = normalizeState(options.state);
            applyRuntime(configData);

            if (powerOn === null) {
                logfile.error(`Invalid power state "${options.state}"`);
                process.exit(exitCodes.commandFailed);
            }

            try {
                await testBoardLink.initSerial(
                    configData.testBoardTerminalDev,
                    configData.serialBaudrate,
                    logfile
                );
                await testBoardLink.targetPower(powerOn);
                logfile.log(JSON.stringify({
                    status: 'OK',
                    errorCode: 0,
                    ErrorDescription: `Power ${powerOn ? 'on' : 'off'}`
                }));
                await delay(100);
                process.exit(exitCodes.normalExit);
            } catch (err) {
                logfile.error(`Power command failed: ${err.message}`);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};